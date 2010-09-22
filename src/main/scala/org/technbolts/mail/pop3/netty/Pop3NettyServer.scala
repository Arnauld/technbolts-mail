package org.technbolts.mail.pop3.netty

import java.util.concurrent.Executors
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.handler.codec.frame.DelimiterBasedFrameDecoder
import org.jboss.netty.handler.codec.string.{StringEncoder, StringDecoder}
import org.slf4j.{LoggerFactory, Logger}
import org.jboss.netty.channel._
import group.{ChannelGroupFuture, ChannelGroup, DefaultChannelGroup}
import java.net.InetSocketAddress
import org.technbolts.util.EventDispatcher
import org.jboss.netty.buffer.ChannelBuffers
import org.technbolts.mail.{User, Mailbox, MailboxRepository}
import org.technbolts.mail.pop3.{UnsupportedCommandException, Pop3Command, Pop3Event}

trait Pop3ServerContext {
  def mailboxRepository:MailboxRepository

  def lockAndGetMailbox(credentials:Credentials) =
    mailboxRepository.lockAndGetMailbox(User(credentials.userName.get, credentials.userPass.get))
}

class Pop3NettyServer(val port: Int, val mailboxRepository:MailboxRepository) extends Pop3ServerContext {
  val logger: Logger = LoggerFactory.getLogger(classOf[Pop3NettyServer])
  val listeners = new EventDispatcher[Pop3Event]

  val allChannels:ChannelGroup = new DefaultChannelGroup("pop3-server")
  var channelFactory:ChannelFactory = _

  def start:Unit = {
    channelFactory = new NioServerSocketChannelFactory(
      Executors.newCachedThreadPool,
      Executors.newCachedThreadPool
      )

    val bootstrap  = new ServerBootstrap(channelFactory)
    bootstrap.setPipelineFactory(new Pop3PipelineFactory(this))
    bootstrap.setParentHandler( new SimpleChannelHandler {
      override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent):Unit =
        allChannels.remove(e.getChannel)
      override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent):Unit =
        allChannels.add(e.getChannel)
    })
    val channel = bootstrap.bind(new InetSocketAddress(port))
    allChannels.add(channel)
    logger.info("POP3 Server running on port <" + port + "> waiting for connection")
  }

  def stop:Unit = {
    logger.info("POP3 Server stop requested")
    val future:ChannelGroupFuture = allChannels.close
    future.awaitUninterruptibly
    channelFactory.releaseExternalResources
  }
}

class Pop3PipelineFactory(val context:Pop3ServerContext) extends ChannelPipelineFactory {

  def crlfDelimiter = ChannelBuffers.wrappedBuffer(Array[Byte]('\r', '\n'))

  def getPipeline: ChannelPipeline = {
    // Create a default pipeline implementation.
    val pipeline: ChannelPipeline = Channels.pipeline

    // Add the text line codec combination first,
    pipeline.addLast("framer", new DelimiterBasedFrameDecoder(8192, crlfDelimiter))
    pipeline.addLast("decoder", new StringDecoder)
    pipeline.addLast("encoder", new StringEncoder)

    // and then business logic.
    pipeline.addLast("handler", new Pop3ServerHandler(context))

    return pipeline;
  }
}

trait Pop3NettyChannelAdapter {
  def logger: Logger

  val CRLF = "\r\n"

  def write(channel: Channel, message: String): Unit = {
    channel.write(message)
    channel.write(CRLF)
    //logger.debug(" > " + message)
  }

  def writeOk(channel: Channel): Unit = {
    channel.write("+OK")
    channel.write(CRLF)
    logger.debug(" > +OK")
  }

  def writeOk(channel: Channel, message: String): Unit = {
    channel.write("+OK " + message)
    channel.write(CRLF)
    logger.debug(" > +OK " + message)
  }

  def writeErr(channel: Channel, message: String): Unit = {
    channel.write("-ERR " + message)
    channel.write(CRLF)
    logger.debug("> -ERR " + message)
  }

  def readLine(e: MessageEvent):String = {
    // Cast to a String first.
    // We know it is a String because we put some codec in Pop3PipelineFactory.
    val line = e.getMessage.asInstanceOf[String]
    line
  }
}

class Pop3ServerHandler(val context:Pop3ServerContext) extends SimpleChannelUpstreamHandler with Pop3NettyChannelAdapter {
  val logger: Logger = LoggerFactory.getLogger(classOf[Pop3ServerHandler])

  override def handleUpstream(ctx: ChannelHandlerContext, e: ChannelEvent): Unit = {
    e match {
      case cse: ChannelStateEvent => logger.debug(cse.toString)
      case _ => //ignore
    }
    super.handleUpstream(ctx, e);
  }

  override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    /*
     * Once the TCP connection has been opened by a POP3 client, the POP3
     * server issues a one line greeting.  This can be any positive
     * response.  An example might be:
     *
     *  S:  +OK POP3 server ready
     */
    writeOk(e.getChannel, "Welcome!")
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit = {
    logger.warn("Unexpected exception from downstream.", e.getCause)
    e.getChannel.close
  }

  var session: Pop3Session = Pop3Session(Credentials(None,None), None, context, AuthorizationHandler())

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = {
    // Cast to a String first.
    // We know it is a String because we put some codec in Pop3PipelineFactory.
    val line = e.getMessage.asInstanceOf[String]
    logger.debug("< " + line)

    // ok... ok... not nice to put the handler in session, but it makes things simpler
    session = session.handler.handle(e, session)
    session.handler match {
      case QuitHandler => e.getChannel.close
      case _ => //nothing special to do here
    }
  }
}

case class Credentials(userName: Option[String], userPass: Option[String]) {
  def withUser(user:String) = Credentials(Some(user), userPass)
  def withPass(pass:String) = Credentials(userName, Some(pass))
  def isSatisfied:Boolean = userName.isDefined && userPass.isDefined
}

case class Pop3Session(
          credentials:Credentials,
          mailbox:Option[Mailbox],
          serverContext:Pop3ServerContext,
          handler: SessionHandler) {
  def withHandler(newHandler: SessionHandler) = Pop3Session(credentials, mailbox, serverContext, newHandler)
  def withCredentials(newCredentials:Credentials) = Pop3Session(newCredentials, mailbox, serverContext, handler)
  def isAuthSatisfied = credentials.isSatisfied
  def lockAndGetMailbox = serverContext.lockAndGetMailbox(credentials) match {
    case None => (Ko, this)
    case some@Some(mbox) => (Ok, Pop3Session(credentials, some, serverContext, TransactionHandler()))
  }
}

case object Ok
case object Ko

sealed trait SessionHandler extends Pop3NettyChannelAdapter {
  import Pop3Command._
  def handle(e: MessageEvent, session:Pop3Session): Pop3Session = getCommand(e) match {
    case Pop3Command(QUIT, _) => session.withHandler(QuitHandler)
    case Pop3Command(what, _) => writeErr(e.getChannel, "Unsupported command: " + what)
      throw new UnsupportedCommandException("Unsupported command: " + what)
  }

  import Pop3Command._
  def getCommand(e: MessageEvent): Pop3Command = {
    val line = readLine(e)
    lineToCommand(line)
  }

  val msgidPattern = """([0-9]+)""".r

  val msgidAndLinePattern = """([0-9]+) ([0-9]+)""".r
}

case object QuitHandler extends SessionHandler with Pop3NettyChannelAdapter {
  val logger: Logger = LoggerFactory.getLogger(classOf[SessionHandler])
  override def handle(e: MessageEvent, session:Pop3Session) = throw new IllegalStateException
}

case class AuthorizationHandler() extends SessionHandler  {
  val logger: Logger = LoggerFactory.getLogger(classOf[AuthorizationHandler])

  import Pop3Command._
  override def handle(e: MessageEvent, session:Pop3Session) =
    (handleUser(e, session)
      orElse handlePass(e, session)
      orElse handleDefault(e, session))(getCommand(e))

  def handleDefault(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case _ =>
        super.handle(e, session)
  }

  def handlePass(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(PASS, None) =>
      writeErr(e.getChannel, "No password provided")
      session
    case Pop3Command(PASS, Some(password)) =>
      val authSession = session.withCredentials(session.credentials.withPass(password))
      authSession.isAuthSatisfied match {
        case true  =>
          authSession.lockAndGetMailbox match {
            case (Ok, newSession) =>
              writeOk(e.getChannel, "Login successful")
              newSession
            case (Ko, newSession) =>
              writeErr(e.getChannel, "Mailbox already locked")
              newSession
          }
        case false =>
          writeErr(e.getChannel, "Invalid credentials")
          authSession
      }
  }

  def handleUser(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(USER, None) =>
      writeErr(e.getChannel, "No user provided")
      session
    case Pop3Command(USER, Some(login)) =>
      writeOk(e.getChannel, "Password required for " + login)
      session.withCredentials(session.credentials.withUser(login))
  }
}

case class TransactionHandler() extends SessionHandler {
  val logger: Logger = LoggerFactory.getLogger(classOf[TransactionHandler])

  import Pop3Command._
  override def handle(e: MessageEvent, session:Pop3Session) =
    (handleStat(e, session)
      orElse handleTop(e, session)
      orElse handleRetr(e, session)
      orElse handleNoop(e, session)
      orElse handleDefault(e, session))(getCommand(e))

  def handleDefault(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case _ =>
        super.handle(e, session)
  }

  def handleRetr(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(RETR, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      session.mailbox.get.getMessage(msgid - 1) match {
        case None => writeErr(e.getChannel, "Message not found")
        case Some(msg) =>
          writeOk(e.getChannel)
          msg.writeTo(writer(e), None)
          write(e.getChannel, ".")
      }
      session
    case Pop3Command(RETR, arg) =>
      writeErr(e.getChannel, "Invalid argument for " + RETR + " requires msgid as numeric values; got: " + arg)
      session
  }

  def handleStat(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(STAT, _) =>
      val mbox = session.mailbox.get
      writeOk(e.getChannel, mbox.getNumberOfMessage + " " + mbox.getSizeOfAllMessage)
      session
  }

  def handleNoop(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(NOOP, _) =>
      writeOk(e.getChannel)
      session
  }

  def writer(e: MessageEvent):(String)=>Unit = (line:String)=>write(e.getChannel, line)

  def handleTop(e: MessageEvent, session:Pop3Session): PartialFunction[Pop3Command, Pop3Session] = {
    case Pop3Command(TOP, Some(msgidAndLinePattern(msgIdStr, nbLinesStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      val nbLines = Integer.parseInt(nbLinesStr)
      session.mailbox.get.getMessage(msgid - 1) match {
        case None =>
          writeErr(e.getChannel, "Message not found")
        case Some(msg) =>
          writeOk(e.getChannel)
          msg.writeTo(writer(e), Some(nbLines))
          write(e.getChannel, ".")
      }
      session
    case Pop3Command(TOP, arg) =>
      writeErr(e.getChannel, "Invalid argument for " + TOP + " requires msgid and line numbers as numeric values; got: " + arg)
      session
  }
}
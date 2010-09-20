package org.technbolts.mail.pop3.netty

import java.util.concurrent.Executors
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.handler.codec.frame.{Delimiters, DelimiterBasedFrameDecoder}
import org.jboss.netty.handler.codec.string.{StringEncoder, StringDecoder}
import org.jboss.netty.buffer.ChannelBuffers
import org.slf4j.{LoggerFactory, Logger}
import org.jboss.netty.channel._
import java.net.InetSocketAddress
import org.technbolts.util.EventDispatcher
import org.technbolts.mail.pop3.{UnsupportedCommandException, Pop3Command, Pop3Event}

class Pop3NettyServer(port: Int) {
  val listeners = new EventDispatcher[Pop3Event]

  def start {
    val channelFactory = new NioServerSocketChannelFactory(
      Executors.newCachedThreadPool,
      Executors.newCachedThreadPool
      )

    val bootstrap = new ServerBootstrap(channelFactory)
    bootstrap.setPipelineFactory(new Pop3PipelineFactory)
    bootstrap.bind(new InetSocketAddress(port))
    logger.info("POP3 Server running on port <" + port + "> waiting for connection")
  }
}

class Pop3PipelineFactory extends ChannelPipelineFactory {
  def crlfDelimiter = Array(ChannelBuffers.wrappedBuffer(Array[Byte]('\r', '\n')))

  def getPipeline: ChannelPipeline = {
    // Create a default pipeline implementation.
    val pipeline: ChannelPipeline = pipeline()

    // Add the text line codec combination first,
    pipeline.addLast("framer", new DelimiterBasedFrameDecoder(8192, crlfDelimiter))
    pipeline.addLast("decoder", new StringDecoder)
    pipeline.addLast("encoder", new StringEncoder)

    // and then business logic.
    pipeline.addLast("handler", new Pop3ServerHandler)

    return pipeline;
  }
}

trait Pop3NettyChannelAdapter {
  val CRLF = "\r\n"

  def writeOk(channel: Channel, message: String): Unit = {
    channel.write("+OK " + message)
    channel.write(CRLF)
  }
  def writeErr(channel: Channel, message: String): Unit = {
    channel.write("-ERR " + message)
    channel.write(CRLF)
  }
}

class Pop3ServerHandler extends SimpleChannelUpstreamHandler with Pop3NettyChannelAdapter {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3ServerHandler])

  def handleUpstream(ctx: ChannelHandlerContext, e: ChannelEvent): Unit = {
    e match {
      case cse: ChannelStateEvent => logger.info(cse.toString)
      case _ => //ignore
    }
    super.handleUpstream(ctx, e);
  }

  def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    /*
     * Once the TCP connection has been opened by a POP3 client, the POP3
     * server issues a one line greeting.  This can be any positive
     * response.  An example might be:
     *
     *  S:  +OK POP3 server ready
     */
    writeOk(e.getChannel, "Welcome!")
  }

  def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit = {
    logger.warn("Unexpected exception from downstream.", e.getCause)
    e.getChannel.close
  }

  var sessionContext:SessionContext = AuthContext(None,None)

  def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = {
    sessionContext = sessionContext.handle(e)
    if (sessionContext.isQuit) {
      e.getChannel.close
    }
  }
}

sealed trait SessionContext {
  
  def handle(e: MessageEvent):SessionContext
  def isQuit = false

  import Pop3Command._
  def getCommand(e: MessageEvent):Pop3Command = {
    // Cast to a String first.
    // We know it is a String because we put some codec in Pop3PipelineFactory.
    val line = e.getMessage.asInstanceOf[String]
    lineToCommand(line)
  }
}

case class QuitContext extends SessionContext with Pop3NettyChannelAdapter {
  override def isQuit = true
  def handle(e: MessageEvent) = throw new IllegalStateException
}

case class AuthorizationContext(userName:Option[String], userPass:Option[String]) extends SessionContext with Pop3NettyChannelAdapter {
  private val logger: Logger = LoggerFactory.getLogger(classOf[AuthorizationContext])

  def handle(e: MessageEvent) = getCommand(e) match {
    case Pop3Command(USER, None) =>
      writeErr(e, "No user provided")
      this
    case Pop3Command(USER, Some(login)) =>
      writeOk("Password required for " + login)
      new AuthorizationContext(Some(login), userPass).nextIfComplete
    case Pop3Command(PASS, None) =>
      writeErr("No password provided")
      this
    case Pop3Command(PASS, Some(password)) =>
      writeOk("Login successful")
      new AuthorizationContext(userName, Some(password)).nextIfComplete
    case Pop3Command(QUIT, _) =>
      new QuitContext
    case Pop3Command(command, _) =>
      writeErr("Unsupported command: " + command)
      this
  }

  def nextIfComplete:SessionContext = if (userName.isDefined && userPass.isDefined)
                                        new TransactionContext
                                      else
                                        this
}

case class TransactionContext() extends SessionContext with Pop3NettyChannelAdapter {
  private val logger: Logger = LoggerFactory.getLogger(classOf[AuthorizationContext])

  def handle(e: MessageEvent) = getCommand(e) match {


    case Pop3Command(QUIT, _) =>
      new QuitContext
    case Pop3Command(command, _) =>
      writeErr("Unsupported command: " + command)
      this
  }
}
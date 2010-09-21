package org.technbolts.mail.smtp.netty

import org.jboss.netty.bootstrap.ServerBootstrap
import java.util.concurrent.Executors
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.handler.codec.frame.{Delimiters, DelimiterBasedFrameDecoder}
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.handler.codec.string.{StringDecoder, StringEncoder}
import org.slf4j.{LoggerFactory, Logger}
import org.jboss.netty.channel._
import java.net.{InetAddress, InetSocketAddress}
import org.technbolts.mail.smtp.SmtpCommand

class SmtpNettyServer(port: Int) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpNettyServer])
  logger.info("Smtp (netty) Server starting on port <" + port + ">")

  def start: Unit = {
    // Configure the server.
    val bootstrap = new ServerBootstrap(
      new NioServerSocketChannelFactory(
        Executors.newCachedThreadPool,
        Executors.newCachedThreadPool))

    // Configure the pipeline factory.
    bootstrap.setPipelineFactory(new SmtpServerPipelineFactory());

    // Bind and start to accept incoming connections.
    bootstrap.bind(new InetSocketAddress(port))
  }
}

class SmtpServerPipelineFactory extends ChannelPipelineFactory {
  def getPipeline: ChannelPipeline = {
    // Create a default pipeline implementation.
    val pipeline = pipeline()

    // Add the text line codec combination first,
    pipeline.addLast("framer", new DelimiterBasedFrameDecoder(8192, crlfDelimiter))
    pipeline.addLast("decoder", new StringDecoder);
    pipeline.addLast("encoder", new StringEncoder);

    // and then business logic.
    pipeline.addLast("handler", new SmtpServerNettyHandler)

    return pipeline;
  }
}

class SmtpServerNettyHandler extends SimpleChannelUpstreamHandler {

  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpServerNettyHandler])

  var session = new SmtpSession(SmtpSession.None)

  override def handleUpstream(ctx: ChannelHandlerContext, e: ChannelEvent): Unit = {
    e match {
      case cse: ChannelStateEvent => logger.info(e.toString());
      case _ => //ignore
    }
    super.handleUpstream(ctx, e);
  }

  override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    // Send greeting for a new connection.
    SmtpSession.writeWelcome(e.getChannel);
    session = new SmtpSession(SmtpSession.Init)
  }

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = {
    session = session.handleCall(SmtpCall(e))
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit = {
    logger.warn("Unexpected exception from downstream.", e.getCause());
    e.getChannel().close();
  }
}

object SmtpSession {
  val None = 0
  val Init = 1
  val Mail = 2
  val Rcpt = 3
  val Data = 3

  val recipientPattern = """<([^ ]+)>[ ]?(.*)""".r

  val crlf = "\r\n"

  def crlfBytes = Array[Byte]('\r', '\n')

  /**
   * Returns    { @code CR ('\r') } and    { @code LF ('\n') } delimiters, which could
   * be used for text-based line protocols.
   */
  def crlfDelimiter: Array[ChannelBuffer] =
    Array[ChannelBuffer](ChannelBuffers.wrappedBuffer(crlfBytes))

  def writeWelcome(channel:Channel):Unit =
    write(channel, "220 " + InetAddress.getLocalHost().getHostName() + " ready!")

  def writeBadSequence(channel:Channel):Unit = write(channel, "503 Bad sequence of commands")

  def writeOk(channel:Channel):Unit = write(channel, "250 OK")

  def write(channel:Channel, message:String):Unit = {
    channel.write(message)
    channel.write(crlf)
  }
}

class SmtpSession(val state:Int, val mail:String, val rcpt:List[String]) {
  import SmtpSession._
  import SmtpCommand._
  def handleCall(e: MessageEvent): SmtpSession = state match {
    case None => throw new IllegalStateException
    case Init => (handleEhlo orElse handleQuit orElse handleRset orElse {
      case SmtpCommand(MAIL, recipientPattern(address)) => new SmtpSession(Mail, address, Nil)
      case SmtpCommand(RCPT, _) => writeBadSequence(e.getChannel) this
      case SmtpCommand(DATA, _) => writeBadSequence(e.getChannel) this
    } orElse handleNoop orElse handleUnsupported) (command(e))
    case Mail => (handleEhlo orElse handleQuit orElse handleRset orElse {
      case SmtpCommand(MAIL, _) => writeBadSequence(e.getChannel) this
      case SmtpCommand(RCPT, recipientPattern(address)) => new SmtpSession(Rcpt, mail, address::rcpt)
      case SmtpCommand(DATA, _) => writeBadSequence(e.getChannel) this
    } orElse handleNoop orElse handleUnsupported) (command(e))
    case Rcpt => (handleEhlo orElse handleQuit orElse handleRset orElse {
      case SmtpCommand(MAIL, _) => writeBadSequence(e.getChannel) this
      case SmtpCommand(RCPT, recipientPattern(address)) => new SmtpSession(Rcpt, mail, address::rcpt)
      case SmtpCommand(DATA, _) => writeOk(e.getChannel); new SmtpSession(Data, mail, rcpt)
    } orElse handleNoop orElse handleUnsupported) (command(e))
    case Data => (handleEhlo orElse handleQuit orElse handleRset orElse {
      case SmtpCommand(MAIL, _) => writeBadSequence(e.getChannel) this
      case SmtpCommand(RCPT, _) => writeBadSequence(e.getChannel) this
      case SmtpCommand(DATA, _) => writeOk(e.getChannel); new SmtpSession(Data, mail, rcpt)
    } orElse handleNoop orElse handleUnsupported) (command(e))
  }

  // Cast to a String first.
  // We know it is a String because we put some codec in SmtpServerPipelineFactory.
  def line(e: MessageEvent) = e.getMessage.asInstanceOf[String]

  //
  //
  def command(e: MessageEvent) = SmtpCommand.stringToCommand(line(e))


  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Smtp commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def handleUnsupported: PartialFunction[SmtpCommand, SmtpSession] = {
    case SmtpCommand(command, _) =>
      write("500 Command Unrecognized: " + command)
  }

  def handleQuit: PartialFunction[SmtpCommand, SmtpSession] = {
    case SmtpCommand(QUIT, _) =>
    case SmtpCommand(NULL, _) =>
      //consider it is a disconnection
  }

  def handleEhlo: PartialFunction[SmtpCommand, SmtpSession] = {
    case SmtpCommand(EHLO, Some(argument)) => helloResponse(argument)
    case SmtpCommand(EHLO, _) => helloResponse("anonymous?")
    case SmtpCommand(HELO, Some(argument)) => helloResponse(argument)
    case SmtpCommand(HELO, _) => helloResponse("anonymous?")
  }

  var helloResponse:(String)=>SmtpSession = (client:String) => {
    write("250 " + serverState.domain + " Greeting " + client + "!")
    this
  }

  def handleNoop: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(NOOP, _) =>
      writeOk
  }

  def handleRset: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(RSET, argument) =>
      transaction = createSmtpTx
      writeOk
  }

  def handleMail: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(MAIL, Some(recipientPattern(mail, extra))) =>
      if(transaction.acceptMail) {
        transaction.mail(mail)
        writeOk
      }
      else
        writeBadSequence
  }

  def handleRcpt: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(RCPT, Some(recipientPattern(mail, extra))) =>
      if(transaction.acceptRcpt) {
        transaction.rcpt(mail)
        writeOk
      }
      else
        writeBadSequence
  }

  def handleData: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(DATA, _) =>
      if(transaction.acceptData) {
        transaction.dataReceived
        write("354 Start mail input; end with <CRLF>.<CRLF>")
        var line = readLine
        while(line!=null && line!=".") {
          transaction.data(line)
          line = readLine
        }
        transaction.commit
        writeOk
      }
      else
        writeBadSequence
  }
}
class InitialSmtpSession extends SmtpSession {
  def handleCall(call:SmtpCall): SmtpSession =
    (handleEhlo orElse handleQuit orElse handleMail) (call.command)
}


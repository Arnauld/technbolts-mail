package org.technbolts.mail.pop3.netty

import java.util.concurrent.Executors
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.handler.codec.frame.DelimiterBasedFrameDecoder
import org.jboss.netty.handler.codec.string.{StringEncoder, StringDecoder}
import org.slf4j.{LoggerFactory, Logger}
import org.jboss.netty.channel._
import group.{ChannelGroupFuture, ChannelGroup, DefaultChannelGroup}
import org.jboss.netty.buffer.ChannelBuffers
import org.technbolts.util.EventDispatcher
import org.technbolts.mail.pop3.{UnsupportedCommandException, Pop3Command}
import java.net.InetSocketAddress
import org.technbolts.mail._

class Pop3NettyServer(val port: Int, val mailboxRepository: MailboxRepository) extends Pop3Server {
  val logger: Logger = LoggerFactory.getLogger(classOf[Pop3NettyServer])
  val listeners = new EventDispatcher[Pop3Event]

  val allChannels: ChannelGroup = new DefaultChannelGroup("pop3-server")
  var channelFactory: ChannelFactory = _

  def start: Unit = {
    logger.info("POP3 Server starting on port <" + port + ">")

    channelFactory = createChannelFactory
    val bootstrap = createBootstrap
    val channel = bootstrap.bind(new InetSocketAddress(port))
    allChannels.add(channel)

    logger.info("POP3 Server running on port <" + port + "> waiting for connection")
    listeners.publishEvent(OnPop3ServerStart(this))
  }

  protected def createChannelFactory = new NioServerSocketChannelFactory(
    Executors.newCachedThreadPool,
    Executors.newCachedThreadPool
    )

  protected def createServerChannelHandler = new SimpleChannelHandler {
    override def channelOpen(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
      allChannels.add(e.getChannel)
      super.channelOpen(ctx, e)
    }

    override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
      /**remote identity */
      val remoteAddress = e.getChannel.getRemoteAddress.asInstanceOf[InetSocketAddress]
      val remoteHost = remoteAddress.getHostName
      val remoteIp = remoteAddress.getAddress
      logger.info("Accepting connection from " + remoteHost + " (" + remoteIp + ")")
      super.channelConnected(ctx, e)
    }

    override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
      allChannels.remove(e.getChannel)
      super.channelClosed(ctx, e)
    }
  }

  protected def createBootstrap = {
    val bootstrap = new ServerBootstrap(channelFactory)
    bootstrap.setPipelineFactory(new Pop3PipelineFactory(this) {
      override def getPipeline = {
        val pipeline = super.getPipeline
        pipeline.addFirst("server-handler", createServerChannelHandler)
        pipeline
      }
    })
    bootstrap
  }

  def stop: Unit = {
    logger.info("POP3 Server stop requested")
    val future: ChannelGroupFuture = allChannels.close
    future.awaitUninterruptibly
    channelFactory.releaseExternalResources
  }
}

class Pop3PipelineFactory(val context: Pop3Server) extends ChannelPipelineFactory {
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

    return pipeline
  }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Netty io adapter
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
trait Pop3IONettyAdapter extends Pop3IO[ChannelEvent] {

  def writeLine(e: ChannelEvent, message: String): Unit = {
    val channel = e.getChannel
    channel.write(message)
    channel.write(CRLF)
    //logger.debug(" > " + message)
  }

  def readLine(e: ChannelEvent): String =
    // Cast to a String first.
    // We know it is a String because we put some codec in Pop3PipelineFactory.
    e.asInstanceOf[MessageEvent].getMessage.asInstanceOf[String]
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Netty channel handler
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Pop3ServerHandler(val context: Pop3Server) extends SimpleChannelUpstreamHandler with Pop3IONettyAdapter {
  val logger: Logger = LoggerFactory.getLogger(classOf[Pop3ServerHandler])

  override def handleUpstream(ctx: ChannelHandlerContext, e: ChannelEvent): Unit = {
    e match {
      case cse: ChannelStateEvent => //logger.debug(cse.toString)
      case _ => //ignore
    }
    super.handleUpstream(ctx, e)
  }

  override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    /*
     * Once the TCP connection has been opened by a POP3 client, the POP3
     * server issues a one line greeting.  This can be any positive
     * response.  An example might be:
     *
     *  S:  +OK POP3 server ready
     */
    writeOk(e, "Welcome!")
    super.channelConnected(ctx, e)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit = {
    logger.warn("Unexpected exception from downstream.", e.getCause)
    e.getChannel.close
  }

  var session: Pop3Session[ChannelEvent] = new Pop3NettySessionAuthorization(Credentials(None, None), context)

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit = {
    // Cast to a String first.
    // We know it is a String because we put some codec in Pop3PipelineFactory.
    val line = e.getMessage.asInstanceOf[String]
    logger.debug("< " + line)

    session = session.handle(e)
    if(session.isClosed)
      e.getChannel.close
    
    super.messageReceived(ctx, e)
  }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Authorization
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Pop3NettySessionAuthorization (
        val credentials: Credentials,
        val serverContext: Pop3Server) extends Pop3SessionAuthorization[ChannelEvent] {

  override def newTransaction(mailbox: Mailbox) =
    new Pop3NettySessionTransaction(mailbox, serverContext)

  override def newAuthorization(newCredentials: Credentials) =
    new Pop3NettySessionAuthorization(newCredentials, serverContext)
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Transaction
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class Pop3NettySessionTransaction(
        val mailbox:Mailbox,
        val serverContext: Pop3Server) extends Pop3SessionTransaction[ChannelEvent] {
}

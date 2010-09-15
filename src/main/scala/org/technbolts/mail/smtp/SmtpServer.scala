package org.technbolts.mail.smtp

import java.net.{SocketTimeoutException, ServerSocket}
import org.slf4j.{Logger, LoggerFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.io._
import org.technbolts.util.LangUtils
import org.technbolts.mail.MailboxRepository
object SmtpServer {
  def apply():SmtpServer = apply(25)
  def apply(port: Int):SmtpServer = apply(port , new File("d:/data/mailboxes"))
  def apply(port: Int, rootDir:File):SmtpServer = apply(port , new MailboxRepository(rootDir))
  def apply(port: Int, mailboxRepository:MailboxRepository):SmtpServer = new SmtpServer(port, mailboxRepository)
}

class SmtpServer(port: Int, mailboxRepository:MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpServer])

  logger.info("Smtp Server starting on port <" + port + ">")
  val serverSocket = new ServerSocket(port)
  val serverState = new SmtpServerState(mailboxRepository)

  def start: Unit = {
    // Set the socket to timeout every 10 seconds so it does not just block forever.
    // and will be aware of a stop notification
    serverSocket.setSoTimeout(1000)

    val idCounter = new AtomicInteger

    logger.info("Smtp Server running on port <" + port + "> waiting for connection")
    while (serverState.isRunning) {
      try {
        val socket = serverSocket.accept

        /**remote identity */
        val remoteAddress = socket.getInetAddress();
        val remoteHost = remoteAddress.getHostName();
        val remoteIp = remoteAddress.getHostAddress();

        logger.info("Accepting connection from " + remoteHost + " (" + remoteIp + ")")

        val pid = "<" + remoteIp + "#" + idCounter.incrementAndGet + ">"

        /**Writer to send data to the client */
        val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

        /**Reader to read data from the client */
        val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

        val onExit = () => {
          logger.debug(pid + " Socket closed")
          socket.close
        }

        new Thread(new SmtpSession(pid, reader, writer, serverState, onExit)).start

      } catch {
        case e: SocketTimeoutException =>
        // nothing to do, we are just waiting for new connection
        // see serverSocket.setSoTimeout
        case e => throw e //rethrow it
      }
    }
  }
}

class SmtpServerState(val mailboxRepository:MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpServerState])

  val running = new AtomicBoolean(true)
  def isRunning = running.get
  def stop: Unit = {
    logger.info("Stop required, bye!")
    running.set(false)
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Server based commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  def handleServerCommands: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand("+STOP", _) =>
      stop
      throw new QuitException
  }
}

class SmtpSession(val pid: String,
                  val reader: BufferedReader,
                  val writer: BufferedWriter,
                  val serverState: SmtpServerState,
                  val onExit: () => Unit) extends ProtocolSupport with Runnable {
  import SmtpCommand._

  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpSession])

  def run: Unit = {
    try {

      write("220 Welcome!")

      val firstCommand = readCommand
      (serverState.handleServerCommands orElse doSmtpLoop)(firstCommand)

    } catch {
      case q: QuitException =>
        logger.info("Quit asked, bye!")
      case e: Exception => logger.error("Humpf: " + e.getMessage, e)
    } finally {
      onExit()
    }
  }

  lazy val smtpPF: PartialFunction[SmtpCommand, Unit] = LangUtils.combine(handleEhlo).orElse(
      handleNoop,
      //handleRset,
      //handleMail,
      //handleRcpt,
      //handleData,
      handleQuit,
      handleUnsupported).get

  def doSmtpLoop: PartialFunction[SmtpCommand, Unit] = {
    case command: SmtpCommand => {
      var cmd = command
      while (true) {
        smtpPF(cmd)
        logger.debug(pid + " waiting for commands")
        cmd = readCommand
      }
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Smtp commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def handleUnsupported: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(command, _) => throw new UnsupportedCommandException("Unsupported command: " + command)
  }

  def handleQuit: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(QUIT, _) => throw new QuitException
    case SmtpCommand(NULL, _) =>
      //consider it is a disconnection
      throw new QuitException
  }

  def handleEhlo: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(EHLO, Some(argument)) =>
      write("250 Hello "+argument)
    case SmtpCommand(EHLO, _) =>
      write("250 Hello anonymous!")
  }

  def handleNoop:PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(NOOP, _) =>
      write("250 OK")
  }

  def handleRset: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(RSET, argument) =>
  }

  def handleMail: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(MAIL, argument) =>
  }

  def handleData: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(DATA, argument) =>
  }
}
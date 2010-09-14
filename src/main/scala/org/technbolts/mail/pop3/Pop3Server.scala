package org.technbolts.mail.pop3

import java.net.{SocketTimeoutException, ServerSocket}
import org.slf4j.{Logger, LoggerFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.io._
import org.technbolts.util.LangUtils
import org.technbolts.mail.{MailboxRepository, Mailbox, User}

object Pop3Server {
  def apply():Pop3Server = apply(110)
  def apply(port: Int):Pop3Server = apply(port , new File("d:/data/mailboxes"))
  def apply(port: Int, rootDir:File):Pop3Server = apply(port , new MailboxRepository(rootDir))
  def apply(port: Int, mailboxRepository:MailboxRepository):Pop3Server = new Pop3Server(port, mailboxRepository)
}

class Pop3Server(port: Int, mailboxRepository:MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3Server])

  logger.info("POP3 Server starting on port <" + port + ">")
  val serverSocket = new ServerSocket(port)
  var running = new AtomicBoolean(true)

  def start: Unit = {
    // Set the socket to timeout every 10 seconds so it does not just block forever.
    // and will be aware of a stop notification
    serverSocket.setSoTimeout(1000)

    val idCounter = new AtomicInteger

    logger.info("POP3 Server running on port <" + port + "> waiting for connection")
    while (running.get) {
      try {
        val socket = serverSocket.accept

        /**remote identity */
        val remoteAddress = socket.getInetAddress();
        val remoteHost = remoteAddress.getHostName();
        val remoteIp = remoteAddress.getHostAddress();

        logger.info("Accepting connection from " + remoteHost + " (" + remoteIp + ")")

        val pid = "<" + remoteIp + "#" + idCounter.incrementAndGet + ">"

        /**Writer to send data to the client */
        val out: PrintWriter = new PrintWriter(socket.getOutputStream(), true)

        /**Reader to read data from the client */
        val in: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

        val onExit = () => {socket.close}
        new Thread(new Pop3Session(pid, in, out, handleServerCommands, mailboxRepository, onExit)).start

      } catch {
        case e: SocketTimeoutException =>
        // nothing to do, we are just waiting for new connection
        // see serverSocket.setSoTimeout
        case e => throw e //rethrow it
      }
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Server based commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  def handleServerCommands: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command("+STOP", _) =>
      stop
      throw new QuitException
  }


  def stop: Unit = {
    logger.info("Stop required, bye!")
    running.set(false)
  }
}

class Pop3Session(val pid: String,
                  val _reader: BufferedReader,
                  val _writer: PrintWriter,
                  val handleServerCommands: PartialFunction[Pop3Command, Unit],
                  val mailboxRepository:MailboxRepository,
                  val onExit: () => Unit) extends ProtocolSupport with Runnable {
  import Pop3Command._

  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3Session])

  def reader = _reader

  def writer = _writer

  var mailbox: Mailbox = _

  def run: Unit = {
    try {

      writeOk("Welcome!")

      val firstCommand = readCommand
      (handleServerCommands orElse doPop3Loop)(firstCommand)

    } catch {
      case q: QuitException =>
        logger.info("Quit asked, bye!")
      case e: Exception => logger.error("Humpf: " + e.getMessage, e)
    } finally {
      onExit()
    }
  }

  def doPop3Loop: PartialFunction[Pop3Command, Unit] = {
    case command: Pop3Command => {
      val user = authenticate(command)
      mailboxRepository.acquireMailbox(user) {
        case None =>  writeErr("Mailbox already locked")
        case Some(mbox) =>
          mailbox = mbox
          while (true)
            handleCommand
      }
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Shared commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def handleUnsupported: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(command, _) => throw new UnsupportedCommandException("Unsupported command: " + command)
  }

  def handleQuit: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(QUIT, _) =>
      if(mailbox!=null) {
        logger.info(pid + " Transaction phase, QUIT triggered: deleting messages marked as deleted")
        mailbox.processDeleted
      }
      throw new QuitException
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Authorization state commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def authenticate(pop3Command: Pop3Command): User = {
    logger.info(pid + " Authorization phase, waiting for user and password commands")

    val user = new User(null, null)
    val authPF = handleUser(user) orElse handlePassword(user) orElse handleQuit orElse handleUnsupported

    var command = pop3Command
    do {
      authPF(command)
      command = if (user.isComplete) null else readCommand
    }
    while (!user.isComplete)
    logger.info(pid + " Authorization phase sucessfull, user: " + user + " entering in transaction state")
    user
  }

  def handleUser(user: User): PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(USER, None) =>
      logger.info(pid + " No user provided")
      writeErr("No user provided")
    case Pop3Command(USER, Some(login)) =>
      logger.info(pid + " Authorization phase for user: " + login)
      writeOk("Password required for " + login)
      user.login = login
  }

  def handlePassword(user: User): PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(PASS, None) =>
      logger.info(pid + " No password provided")
      writeErr("No password provided")
    case Pop3Command(PASS, Some(password)) =>
      writeOk("Login successful")
      user.password = password
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Transaction state commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  def handleCommand: Unit = {
    logger.debug(pid + " Transaction phase, waiting for commands")
    transactionPF(readCommand)
  }

  def transactionPF: PartialFunction[Pop3Command, Unit] = LangUtils.combine(handleStat,
    List(
      //handleList,
      handleRetr,
      handleDele,
      handleNoop,
      handleRset,
      handleTop,
      //handleUidl,
      handleQuit,
      handleUnsupported))

  def handleStat: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(STAT, _) =>
      writeOk(mailbox.getNumberOfMessage + " " + mailbox.getSizeOfAllMessage);
  }

  def handleList: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(LIST, _) =>
  }

  val msgidPattern = """([0-9]+)""".r
  def handleRetr: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RETR, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid-1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          writeOk
          msg.writeTo(writer, None)
          write(EOT)
      }
    case Pop3Command(RETR, arg) =>
      writeErr("Invalid argument for " + RETR + " requires msgid as numeric values; got: " + arg)
  }

  def handleDele: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(DELE, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid-1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          if(msg.isDeleted)
            writeErr("Message already deleted.")
          else {
            msg.delete
            writeOk
          }
      }
    case Pop3Command(DELE, arg) =>
      writeErr("Invalid argument for " + DELE + " requires msgid as numeric values; got: " + arg)
  }

  def handleNoop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(NOOP, _) => writeOk
  }

  def handleRset: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RSET, _) => mailbox.messages.foreach( _.undelete )
  }

  val  msgidAndLinePattern = """([0-9]+) ([0-9]+)""".r

  def handleTop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(TOP, Some(msgidAndLinePattern(msgIdStr, nbLinesStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      val nbLines = Integer.parseInt(nbLinesStr)
      mailbox.getMessage(msgid-1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          writeOk
          msg.writeTo(writer, Some(nbLines))
          write(EOT)
      }
    case Pop3Command(TOP, arg) =>
      writeErr("Invalid argument for " + TOP + " requires msgid and line numbers as numeric values; got: " + arg)
  }

  val encoding = "iso-8859-1"

  def handleUidl: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(UIDL, Some(msgidPattern(msgIdStr))) =>
        val msgid = Integer.parseInt(msgIdStr)
        mailbox.getMessage(msgid-1) match {
          case None => writeErr("Message not found")
          case Some(msg) =>
            if(msg.isDeleted)
              writeErr("Message deleted.")
            else {
              writeOk(msgid+" "+msg.uniqueId)
            }
        }
    case Pop3Command(UIDL, _) =>
  }
}
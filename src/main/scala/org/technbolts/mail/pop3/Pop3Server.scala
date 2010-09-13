package org.technbolts.mail.pop3

import java.net.{SocketTimeoutException, ServerSocket}
import org.technbolts.mail.{Mailbox, User}
import org.slf4j.{Logger, LoggerFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import org.apache.commons.io.IOUtils
import java.io._

object Pop3Server {
  def apply(port: Int) = new Pop3Server(port)

  def apply() = new Pop3Server(110)
}

class Pop3Server(port: Int) {
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

        new Thread(new Pop3Handler(this, pid, in, out, () => {socket.close})).start

      } catch {
        case e: SocketTimeoutException =>
        // nothing to do, we are just waiting for new connection
        // see serverSocket.setSoTimeout
        case e => throw e //rethrow it
      }
    }
  }

  def stop:Unit = {
    logger.info("Stop required, bye!")
    running.set(false)
  }
}

class Pop3Handler(server:Pop3Server, pid: String, _reader: BufferedReader, _writer: PrintWriter, val onExit: () => Unit) extends ProtocolSupport with Runnable {
  import Pop3Commands._

  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3Handler])

  def reader = _reader

  def writer = _writer

  var mailbox: Mailbox = _

  def run: Unit = {
    try {
      writeOk("Welcome!")

      val firstCommand = readCommand
      (readServerCommands orElse handlePop3Command)(firstCommand)

    } catch {
      case q: QuitException => logger.info("Quit asked, bye!")
      case e: Exception => logger.error("Humpf: " + e.getMessage, e)
    } finally {
      onExit()
    }
  }

  def handlePop3Command: PartialFunction[Pop3Command, Unit] = {
    case command:Pop3Command => {
      val user = authenticate(command)
      mailbox = new Mailbox(user, new File("D:\\data\\mailboxes"))
      logger.info("Mailbox for user " + user.login +" opened in: " + mailbox.mailboxDir.getAbsolutePath)
      while (true)
        handleCommand
    }
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Server based commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  def readServerCommands: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command("+STOP", _) =>
      server.stop
      throw new QuitException
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Shared commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def readUnsupported: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(command, _) => throw new UnsupportedCommandException("Unsupported command: " + command)
  }

  def readQuit: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(QUIT, _) => throw new QuitException
  }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Authorization state commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  def authenticate(pop3Command:Pop3Command): User = {
    logger.info(pid + " Authorization phase, waiting for user and password commands")

    val user = new User(null, null)
    val authPF = readUser(user) orElse readPassword(user) orElse readQuit orElse readUnsupported

    var command = pop3Command
    do {
      authPF(command)
      command = if(user.isComplete) null else readCommand
    }
    while(!user.isComplete)
    logger.info(pid + " Authorization phase sucessfull, user: " + user + " entering in transaction state")
    user
  }

  def readUser(user: User): PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(USER, None) =>
      logger.info(pid + " No user provided")
      writeErr("No user provided")
    case Pop3Command(USER, Some(login)) =>
      logger.info(pid + " Authorization phase for user: " + login)
      writeOk("Password required for " + login)
      user.login = login
  }

  def readPassword(user: User): PartialFunction[Pop3Command, Unit] = {
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
    logger.info(pid + " Transaction phase, waiting for commands")
    txPF(readCommand)
  }

  def txPF: PartialFunction[Pop3Command, Unit] =
    List(
      //readList,
      //readRetr,
      //readDele,
      readNoop,
      //readRset,
      readTop,
      //readUidl,
      readQuit,
      readUnsupported)
            .foldLeft(readStat)((acc, x) => acc.orElse(x))

  def readStat: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(STAT, _) =>
      writeOk(mailbox.getNumberOfMessage + " " + mailbox.getSizeOfAllMessage);
  }

  def readList: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(LIST, _) =>
  }

  def readRetr: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RETR, _) =>
  }

  def readDele: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(DELE, _) =>
  }

  def readNoop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(NOOP, _) => writeOk
  }

  def readRset: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RSET, _) =>
  }

  val topPattern = """([0-9]+) ([0-9]+)""".r
  def readTop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(TOP, args) =>
      args match {
        case topPattern(msgIdStr, nbLinesStr) =>
            val msgid = Integer.parseInt(msgIdStr)
            val nbLines = Integer.parseInt(nbLinesStr)
            mailbox.getMessage(msgid) match {
              case None => writeErr("Message not found")
              case file:File =>
                writeOk
                val lines = IOUtils.readLines(new FileInputStream(file), encoding)

            }

        case _ => writeErr("Invalid argument for top requires: msgid and line numbers")
      }
  }

  val encoding = "iso-8859-1"

  def readUidl: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(UIDL, _) =>
  }
}
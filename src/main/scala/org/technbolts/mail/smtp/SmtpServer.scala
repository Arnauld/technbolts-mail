package org.technbolts.mail.smtp

import org.slf4j.{Logger, LoggerFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.io._
import org.technbolts.mail.MailboxRepository
import java.net.{SocketTimeoutException, ServerSocket}
import org.technbolts.util.{EventDispatcher, LangUtils}
import collection.mutable.ListBuffer
import org.technbolts.mail.store.ResourceRepository
import org.apache.commons.io.{FileUtils, IOUtils}

object SmtpServer {
  def apply(): SmtpServer = apply(25)

  def apply(port: Int): SmtpServer = apply(port, new File("d:/data/mailboxes"))

  def apply(port: Int, rootDir: File): SmtpServer = apply(port, new MailboxRepository(rootDir))

  def apply(port: Int, mailboxRepository: MailboxRepository): SmtpServer = new SmtpServer(port, mailboxRepository)
}

class SmtpServer(val port: Int, val mailboxRepository: MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpServer])

  logger.info("Smtp Server starting on port <" + port + ">")
  val listeners = new EventDispatcher[SmtpEvent]

  val serverState = new SmtpServerState(mailboxRepository)

  def start: Unit = {
    val serverSocket = new ServerSocket(port)

    // Set the socket to timeout every 10 seconds so it does not just block forever.
    // and will be aware of a stop notification
    serverSocket.setSoTimeout(1000)

    logger.info("Smtp Server running on port <" + port + "> waiting for connection")
    try {
      listeners.publishEvent(OnSmtpServerStart(this))
      doLoop(serverSocket)
    }
    finally {
      listeners.publishEvent(OnSmtpServerStop(this))
    }
  }

  private def doLoop(serverSocket: ServerSocket): Unit = {
    val idCounter = new AtomicInteger

    while (serverState.isRunning) {
      try {
        val socket = serverSocket.accept
        listeners.publishEvent(OnSmtpSessionPreInit(this, socket))

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

  def stop: Unit = {
    serverState.stop
  }
}

trait SmtpContext {
  def mailboxRepository: MailboxRepository
  def resourceRepository: ResourceRepository
}

class SmtpServerState(val mailboxRepository: MailboxRepository) extends SmtpContext {
  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpServerState])

  val running = new AtomicBoolean(true)

  var domain = "127.0.0.1"

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

class SmtpTx(context:SmtpContext) {
  val sMail = 1
  val sRcpt = 2
  val sData = 3
  val sCommitted = 4

  private var mail: Option[String] = None
  private var recipients: List[String] = Nil
  private var state = sMail
  private val data = context.resourceRepository.temporaryResource

  def mail(mail:String):SmtpTx = {
    this.mail = Some(mail)
    this.state = sRcpt
    this
  }

  def rcpt(rcpt: String):SmtpTx = {
    recipients = rcpt :: recipients
    this
  }

  def dataReceived:SmtpTx = {
    this.state = sData
    this
  }

  def data(data: String):SmtpTx = {
    this.data.append(data.getBytes("iso-8859-1"))
    this
  }

  def acceptMail = (state==sMail)
  def acceptRcpt = (state==sRcpt)
  def acceptData = (state==sRcpt || state==sData)

  def commit:Unit = {
    // prevent any further usage
    state = sCommitted

    val file = new File("D:\\temp\\w" + System.currentTimeMillis + ".eml")
    val out = new FileOutputStream(file)
    try{
      data.copyTo(out)
    }
    finally{
      IOUtils.closeQuietly(out)
    }
  }
}

object SmtpSession {
  val recipientPattern = """<([^ ]+)>[ ]?(.*)""".r
}

/*

Command	Reply Codes
HELO or EHLO	250 500 501 503
MAIL FROM	250 500 503
RCPT TO	250 500 503
DATA	354 451
QUIT	221


 */


class SmtpSession(val pid: String,
                  val reader: BufferedReader,
                  val writer: BufferedWriter,
                  val serverState: SmtpServerState,
                  val onExit: () => Unit) extends ProtocolSupport with Runnable {
  import SmtpCommand._
  import SmtpSession._

  private val logger: Logger = LoggerFactory.getLogger(classOf[SmtpSession])

  var transaction = createSmtpTx

  def createSmtpTx:SmtpTx = new SmtpTx (serverState)

  def run: Unit = {
    try {

      write("220 Welcome!")

      val firstCommand = readCommand
      (serverState.handleServerCommands orElse doSmtpLoop)(firstCommand)

    }
    catch {
      case q: QuitException =>
        logger.info("Quit asked, bye!")
      case e: Exception => logger.error("Humpf: " + e.getMessage, e)
    }
    finally {
      onExit()
    }
  }

  lazy val smtpPF: PartialFunction[SmtpCommand, Unit] = LangUtils.combine(handleEhlo).orElse(
    handleNoop,
    handleRset,
    handleMail,
    handleRcpt,
    handleData,
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
    case SmtpCommand(command, _) =>
      write("500 Command Unrecognized: " + command)
  }

  def handleQuit: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(QUIT, _) => throw new QuitException
    case SmtpCommand(NULL, _) =>
      //consider it is a disconnection
      throw new QuitException
  }

  /**
   * <pre>
   *   These commands are used to identify the SMTP client to the SMTP
   *   server.  The argument clause contains the fully-qualified domain name
   *   of the SMTP client, if one is available. 
   * </pre>
   */
  def handleEhlo: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(EHLO, Some(argument)) => helloResponse(argument)
    case SmtpCommand(EHLO, _) => helloResponse("anonymous?")
    case SmtpCommand(HELO, Some(argument)) => helloResponse(argument)
    case SmtpCommand(HELO, _) => helloResponse("anonymous?")
  }

  var helloResponse:(String)=>Unit = (client:String) => {
    write("250 " + serverState.domain + " Greeting " + client + "!")
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

  /**
   * <pre>
   * This command is used to initiate a mail transaction in which the mail
   * data is delivered to an SMTP server that may, in turn, deliver it to
   * one or more mailboxes or pass it on to another system (possibly using
   * SMTP).  The argument clause contains a reverse-path and may contain
   * optional parameters.  In general, the MAIL command may be sent only
   * when no mail transaction is in progress, see Section 4.1.4.
   *
   * The reverse-path consists of the sender mailbox.  Historically, that
   * mailbox might optionally have been preceded by a list of hosts, but
   * that behavior is now deprecated (see Appendix C).  In some types of
   * reporting messages for which a reply is likely to cause a mail loop
   * (for example, mail delivery and non-delivery notifications), the
   * reverse-path may be null (see Section 3.6).
   *
   * This command clears the reverse-path buffer, the forward-path buffer,
   * and the mail data buffer, and it inserts the reverse-path information
   * from its argument clause into the reverse-path buffer.
   *
   * If service extensions were negotiated, the MAIL command may also
   * carry parameters associated with a particular service extension.
   *
   * Syntax:
   *
   * mail = "MAIL FROM:" Reverse-path
   *                                    [SP Mail-parameters] CRLF
   * </pre>
   */
  def handleMail: PartialFunction[SmtpCommand, Unit] = {
    case SmtpCommand(MAIL, Some(recipientPattern(mail, extra))) =>
      if(transaction.acceptMail) {
        transaction.mail(mail)
        writeOk
      }
      else
        writeBadSequence
  }

  /**
   * <pre>
   * This command is used to identify an individual recipient of the mail
   * data; multiple recipients are specified by multiple uses of this
   * command.  The argument clause contains a forward-path and may contain
   * optional parameters.
   *
   * The forward-path normally consists of the required destination
   * mailbox.  Sending systems SHOULD NOT generate the optional list of
   * hosts known as a source route.  Receiving systems MUST recognize
   * source route syntax but SHOULD strip off the source route
   * specification and utilize the domain name associated with the mailbox
   * as if the source route had not been provided.
   *
   * Similarly, relay hosts SHOULD strip or ignore source routes, and
   * names MUST NOT be copied into the reverse-path.  When mail reaches
   * its ultimate destination (the forward-path contains only a
   * destination mailbox), the SMTP server inserts it into the destination
   * mailbox in accordance with its host mail conventions.
   *
   * This command appends its forward-path argument to the forward-path
   * buffer; it does not change the reverse-path buffer nor the mail data
   * buffer.
   *
   * For example, mail received at relay host xyz.com with envelope
   * commands
   *
   *    MAIL FROM:<userx@y.foo.org>
   *    RCPT TO:<@hosta.int,@jkl.org:userc@d.bar.org>
   *
   * will normally be sent directly on to host d.bar.org with envelope
   * commands
   *
   *    MAIL FROM:<userx@y.foo.org>
   *    RCPT TO:<userc@d.bar.org>
   *
   * As provided in Appendix C, xyz.com MAY also choose to relay the
   * message to hosta.int, using the envelope commands
   *
   *    MAIL FROM:<userx@y.foo.org>
   *    RCPT TO:<@hosta.int,@jkl.org:userc@d.bar.org>
   *
   *
   * or to jkl.org, using the envelope commands
   *
   *    MAIL FROM:<userx@y.foo.org>
   *    RCPT TO:<@jkl.org:userc@d.bar.org>
   *
   * Attempting to use relaying this way is now strongly discouraged.
   * Since hosts are not required to relay mail at all, xyz.com MAY also
   * reject the message entirely when the RCPT command is received, using
   * a 550 code (since this is a "policy reason").
   *
   * If service extensions were negotiated, the RCPT command may also
   * carry parameters associated with a particular service extension
   * offered by the server.  The client MUST NOT transmit parameters other
   * than those associated with a service extension offered by the server
   * in its EHLO response.
   *
   * Syntax:
   *
   *    rcpt = "RCPT TO:" ( "<Postmaster@" Domain ">" / "<Postmaster>" /
   *                Forward-path ) [SP Rcpt-parameters] CRLF
   *
   *           Note that, in a departure from the usual rules for
   *                local-parts, the "Postmaster" string shown above is
   *                treated as case-insensitive.
   * </pre>
   */
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
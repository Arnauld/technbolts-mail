package org.technbolts.mail.pop3

import java.net.{SocketTimeoutException, ServerSocket}
import org.slf4j.{Logger, LoggerFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.io._
import org.technbolts.util.LangUtils
import org.technbolts.mail.{Message, MailboxRepository, Mailbox, User}

object Pop3Server {
  def apply(): Pop3Server = apply(110)

  def apply(port: Int): Pop3Server = apply(port, new File("d:/data/mailboxes"))

  def apply(port: Int, rootDir: File): Pop3Server = apply(port, new MailboxRepository(rootDir))

  def apply(port: Int, mailboxRepository: MailboxRepository): Pop3Server = new Pop3Server(port, mailboxRepository)
}

class Pop3Server(port: Int, mailboxRepository: MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3Server])

  logger.info("POP3 Server starting on port <" + port + ">")
  val serverSocket = new ServerSocket(port)
  val state = new Pop3ServerState(mailboxRepository)

  def start: Unit = {
    // Set the socket to timeout every 10 seconds so it does not just block forever.
    // and will be aware of a stop notification
    serverSocket.setSoTimeout(1000)

    val idCounter = new AtomicInteger

    logger.info("POP3 Server running on port <" + port + "> waiting for connection")
    while (state.isRunning) {
      try {
        val socket = serverSocket.accept

        /**remote identity */
        val remoteAddress = socket.getInetAddress();
        val remoteHost = remoteAddress.getHostName();
        val remoteIp = remoteAddress.getHostAddress();

        logger.info("Accepting connection from " + remoteHost + " (" + remoteIp + ")")

        val pid = "<" + remoteIp + "#" + idCounter.incrementAndGet + ">"

        /**Writer to send data to the client */
        val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))

        /**Reader to read data from the client */
        val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

        val onExit = () => {socket.close}
        new Thread(new Pop3Session(pid, state, reader, writer, onExit)).start

      } catch {
        case e: SocketTimeoutException =>
        // nothing to do, we are just waiting for new connection
        // see serverSocket.setSoTimeout
        case e => throw e //rethrow it
      }
    }
  }

  def stop: Unit = {
    state.stop
  }
}

class Pop3ServerState(val mailboxRepository: MailboxRepository) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3ServerState])

  val running = new AtomicBoolean(true)

  def stop: Unit = {
    logger.info("Stop required, bye!")
    running.set(false)
  }

  def isRunning:Boolean = running.get

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Server based commands
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  def handleServerCommands: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command("+STOP", _) =>
      stop
      throw new QuitException
  }
}

class Pop3Session(val pid: String,
                  val serverState:Pop3ServerState,
                  _reader: BufferedReader,
                  _writer: BufferedWriter,
                  val onExit: () => Unit) extends ProtocolSupport with Runnable {

  def reader = _reader
  def writer = _writer

  import Pop3Command._

  private val logger: Logger = LoggerFactory.getLogger(classOf[Pop3Session])

  var mailbox: Mailbox = _

  val encoding = "iso-8859-1"

  /**
   * Session thread entry point
   * Runnable#run
   */
  def run: Unit = {
    try {

      /*
       * Once the TCP connection has been opened by a POP3 client, the POP3
       * server issues a one line greeting.  This can be any positive
       * response.  An example might be:
       *
       *  S:  +OK POP3 server ready
       */
      writeOk("Welcome!")

      val firstCommand = readCommand
      (serverState.handleServerCommands orElse doPop3Loop)(firstCommand)

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
      /*
       * The POP3 session is now in the AUTHORIZATION state.  The client must
       * now identify and authenticate itself to the POP3 server.  Two
       * possible mechanisms for doing this are described in this document,
       * the USER and PASS command combination and the APOP command.  Both
       * mechanisms are described later in this document.  Additional
       * authentication mechanisms are described in [RFC1734].  While there is
       * no single authentication mechanism that is required of all POP3
       * servers, a POP3 server must of course support at least one
       * authentication mechanism.
       */
      val user = authenticate(command)

      /*
       * Once the POP3 server has determined through the use of any
       * authentication command that the client should be given access to the
       * appropriate maildrop, the POP3 server then acquires an exclusive-
       * access lock on the maildrop, as necessary to prevent messages from
       * being modified or removed before the session enters the UPDATE state.
       * If the lock is successfully acquired, the POP3 server responds with a
       * positive status indicator.  The POP3 session now enters the
       * TRANSACTION state, with no messages marked as deleted.  If the
       * maildrop cannot be opened for some reason (for example, a lock can
       * not be acquired, the client is denied access to the appropriate
       * maildrop, or the maildrop cannot be parsed), the POP3 server responds
       * with a negative status indicator.  (If a lock was acquired but the
       * POP3 server intends to respond with a negative status indicator, the
       * POP3 server must release the lock prior to rejecting the command.)
       * After returning a negative status indicator, the server may close the
       * connection.  If the server does not close the connection, the client
       * may either issue a new authentication command and start again, or the
       * client may issue the QUIT command.
       *
       * After the POP3 server has opened the maildrop, it assigns a message-
       * number to each message, and notes the size of each message in octets.
       * The first message in the maildrop is assigned a message-number of
       * "1", the second is assigned "2", and so on, so that the nth message
       * in a maildrop is assigned a message-number of "n".  In POP3 commands
       * and responses, all message-numbers and message sizes are expressed in
       * base-10 (i.e., decimal).
       */
      serverState.mailboxRepository.acquireMailbox(user) {
        case None => writeErr("Mailbox already locked")
        case Some(mbox) =>
          mailbox = mbox
          /*
           * loop while server itself is running,
           * Quit is done by throwing a QuitException
           */
          while (serverState.isRunning)
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

  /**
   * <pre>
   * When the client issues the QUIT command from the TRANSACTION state,
   * the POP3 session enters the UPDATE state.  (Note that if the client
   * issues the QUIT command from the AUTHORIZATION state, the POP3
   * session terminates but does NOT enter the UPDATE state.)
   *
   * If a session terminates for some reason other than a client-issued
   * QUIT command, the POP3 session does NOT enter the UPDATE state and
   * MUST not remove any messages from the maildrop.
   *
   *  QUIT
   *
   *     Arguments: none
   *
   *     Restrictions: none
   *
   *     Discussion:
   *         <b>The POP3 server removes all messages marked as deleted
   *         from the maildrop and replies as to the status of this
   *         operation.</b>  If there is an error, such as a resource
   *         shortage, encountered while removing messages, the
   *         maildrop may result in having some or none of the messages
   *         marked as deleted be removed.  In no case may the server
   *         remove any messages not marked as deleted.
   *
   *         Whether the removal was successful or not, the server
   *         then releases any exclusive-access lock on the maildrop
   *         and closes the TCP connection.
   * </pre>
   */
  def handleQuit: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(QUIT, _) =>
      /*
       * When the client issues the QUIT command from the TRANSACTION state,
       * the POP3 session enters the UPDATE state... The POP3 server removes
       * all messages marked as deleted from the maildrop and replies as to
       * the status of this operation.
       */
      if (mailbox != null) {
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

  /**
   * <pre>
   * USER name
   *
   *      Arguments:
   *          a string identifying a mailbox (required), which is of
   *          significance ONLY to the server
   *
   *      Restrictions:
   *          may only be given in the AUTHORIZATION state after the POP3
   *          greeting or after an unsuccessful USER or PASS command
   *
   *      Discussion:
   *          To authenticate using the USER and PASS command
   *          combination, the client must first issue the USER
   *          command.  If the POP3 server responds with a positive
   *          status indicator ("+OK"), then the client may issue
   *          either the PASS command to complete the authentication,
   *          or the QUIT command to terminate the POP3 session.  If
   *          the POP3 server responds with a negative status indicator
   *          ("-ERR") to the USER command, then the client may either
   *          issue a new authentication command or may issue the QUIT
   *          command.
   *
   *          The server may return a positive response even though no
   *          such mailbox exists.  The server may return a negative
   *          response if mailbox exists, but does not permit plaintext
   *          password authentication.
   *
   *     Possible Responses:
   *          +OK name is a valid mailbox
   *          -ERR never heard of mailbox name
   *
   *      Examples:
   *          C: USER frated
   *          S: -ERR sorry, no mailbox for frated here
   *             ...
   *          C: USER mrose
   *          S: +OK mrose is a real hoopy frood
   * </pre>
   */
  def handleUser(user: User): PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(USER, None) =>
      logger.info(pid + " No user provided")
      writeErr("No user provided")
    case Pop3Command(USER, Some(login)) =>
      logger.info(pid + " Authorization phase for user: " + login)
      writeOk("Password required for " + login)
      user.login = login
  }

  /**
   * <pre>
   * PASS string
   *
   *      Arguments:
   *          a server/mailbox-specific password (required)
   *
   *      Restrictions:
   *          may only be given in the AUTHORIZATION state immediately
   *          after a successful USER command
   *
   *      Discussion:
   *          When the client issues the PASS command, the POP3 server
   *          uses the argument pair from the USER and PASS commands to
   *          determine if the client should be given access to the
   *          appropriate maildrop.
   *
   *          Since the PASS command has exactly one argument, a POP3
   *          server may treat spaces in the argument as part of the
   *          password, instead of as argument separators.
   *
   *      Possible Responses:
   *          +OK maildrop locked and ready
   *          -ERR invalid password
   *          -ERR unable to lock maildrop
   *
   *      Examples:
   *          C: USER mrose
   *          S: +OK mrose is a real hoopy frood
   *          C: PASS secret
   *          S: -ERR maildrop already locked
   *            ...
   *          C: USER mrose
   *          S: +OK mrose is a real hoopy frood
   *          C: PASS secret
   *          S: +OK mrose's maildrop has 2 messages (320 octets)
   * </pre>
   */
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

  /**
   * Once the client has successfully identified itself to the POP3 server
   * and the POP3 server has locked and opened the appropriate maildrop,
   * the POP3 session is now in the TRANSACTION state.  The client may now
   * issue any of the following POP3 commands repeatedly.  After each
   * command, the POP3 server issues a response.  Eventually, the client
   * issues the QUIT command and the POP3 session enters the UPDATE state.
   */
  def transactionPF: PartialFunction[Pop3Command, Unit] = LangUtils.combine(handleStat,
    List(
      handleList,
      handleRetr,
      handleDele,
      handleNoop,
      handleRset,
      handleTop,
      handleUidl,
      handleQuit,
      handleUnsupported))

  /**
   * <pre>
   * STAT
   *
   *     Arguments: none
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         The POP3 server issues a positive response with a line
   *         containing information for the maildrop.  This line is
   *         called a "drop listing" for that maildrop.
   *
   *         In order to simplify parsing, all POP3 servers are
   *         required to use a certain format for drop listings.  The
   *         positive response consists of "+OK" followed by a single
   *         space, the number of messages in the maildrop, a single
   *         space, and the size of the maildrop in octets.  This memo
   *         makes no requirement on what follows the maildrop size.
   *         Minimal implementations should just end that line of the
   *         response with a CRLF pair.  More advanced implementations
   *         may include other information.
   *
   *            NOTE: This memo STRONGLY discourages implementations
   *            from supplying additional information in the drop
   *            listing.  Other, optional, facilities are discussed
   *            later on which permit the client to parse the messages
   *            in the maildrop.
   *
   *         Note that messages marked as deleted are not counted in
   *         either total.
   *
   *     Possible Responses:
   *         +OK nn mm
   *
   *     Examples:
   *         C: STAT
   *         S: +OK 2 320
   * </pre>
   */
  def handleStat: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(STAT, _) =>
      writeOk(mailbox.getNumberOfMessage + " " + mailbox.getSizeOfAllMessage);
  }

  /**
   * <pre>
   * LIST [msg]
   *
   *     Arguments:
   *         a message-number (optional), which, if present, may NOT
   *         refer to a message marked as deleted
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         If an argument was given and the POP3 server issues a
   *         positive response with a line containing information for
   *         that message.  This line is called a "scan listing" for
   *         that message.
   *
   *         If no argument was given and the POP3 server issues a
   *         positive response, then the response given is multi-line.
   *         After the initial +OK, for each message in the maildrop,
   *         the POP3 server responds with a line containing
   *         information for that message.  This line is also called a
   *         "scan listing" for that message.  If there are no
   *         messages in the maildrop, then the POP3 server responds
   *         with no scan listings--it issues a positive response
   *         followed by a line containing a termination octet and a
   *         CRLF pair.
   *
   *         In order to simplify parsing, all POP3 servers are
   *         required to use a certain format for scan listings.  A
   *         scan listing consists of the message-number of the
   *         message, followed by a single space and the exact size of
   *         the message in octets.  Methods for calculating the exact
   *         size of the message are described in the "Message Format"
   *         section below.  This memo makes no requirement on what
   *         follows the message size in the scan listing.  Minimal
   *         implementations should just end that line of the response
   *         with a CRLF pair.  More advanced implementations may
   *         include other information, as parsed from the message.
   *
   *            NOTE: This memo STRONGLY discourages implementations
   *            from supplying additional information in the scan
   *            listing.  Other, optional, facilities are discussed
   *            later on which permit the client to parse the messages
   *            in the maildrop.
   *
   *         Note that messages marked as deleted are not listed.
   *
   *     Possible Responses:
   *         +OK scan listing follows
   *         -ERR no such message
   *
   *     Examples:
   *         C: LIST
   *         S: +OK 2 messages (320 octets)
   *         S: 1 120
   *         S: 2 200
   *         S: .
   *           ...
   *         C: LIST 2
   *         S: +OK 2 200
   *           ...
   *         C: LIST 3
   *         S: -ERR no such message, only 2 messages in maildrop
   * </pre>
   */
  def handleList: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(LIST, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          if (msg.isDeleted)
            writeErr("Message deleted.")
          else {
            writeOk(msgid + " " + msg.size)
          }
      }
    case Pop3Command(LIST, _) =>
      writeOk(mailbox.getNumberOfMessage + " messages (" + mailbox.getSizeOfAllMessage + " octets) ")
      var index = 1
      mailbox.messages.foreach((m: Message) => {
        writeOk(index + " " + m.size)
        index = index + 1
      })
      write(EOT)
  }

  val msgidPattern = """([0-9]+)""".r

  /**
   * <pre>
   * RETR msg
   *
   *     Arguments:
   *         a message-number (required) which may NOT refer to a
   *         message marked as deleted
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         If the POP3 server issues a positive response, then the
   *         response given is multi-line.  After the initial +OK, the
   *         POP3 server sends the message corresponding to the given
   *         message-number, being careful to byte-stuff the termination
   *         character (as with all multi-line responses).
   *
   *     Possible Responses:
   *         +OK message follows
   *         -ERR no such message
   *
   *     Examples:
   *         C: RETR 1
   *         S: +OK 120 octets
   *         S: <the POP3 server sends the entire message here>
   *         S: .
   * </pre>
   */
  def handleRetr: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RETR, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          writeOk
          msg.writeTo(writer, None)
          write(EOT)
      }
    case Pop3Command(RETR, arg) =>
      writeErr("Invalid argument for " + RETR + " requires msgid as numeric values; got: " + arg)
  }

  /**
   * <pre>
   * DELE msg
   *
   *     Arguments:
   *         a message-number (required) which may NOT refer to a
   *         message marked as deleted
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         The POP3 server marks the message as deleted.  Any future
   *          reference to the message-number associated with the message
   *         in a POP3 command generates an error.  The POP3 server does
   *         not actually delete the message until the POP3 session
   *         enters the UPDATE state.
   *
   *     Possible Responses:
   *         +OK message deleted
   *         -ERR no such message
   *
   *     Examples:
   *         C: DELE 1
   *         S: +OK message 1 deleted
   *            ...
   *         C: DELE 2
   *         S: -ERR message 2 already deleted
   * </pre>
   */
  def handleDele: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(DELE, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          if (msg.isDeleted)
            writeErr("Message already deleted.")
          else {
            msg.delete
            writeOk
          }
      }
    case Pop3Command(DELE, arg) =>
      writeErr("Invalid argument for " + DELE + " requires msgid as numeric values; got: " + arg)
  }

  /**
   * <pre>
   * NOOP
   *
   *     Arguments: none
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         The POP3 server does nothing, it merely replies with a
   *         positive response.
   *
   *     Possible Responses:
   *         +OK
   *
   *     Examples:
   *         C: NOOP
   *         S: +OK
   * </pre>
   */
  def handleNoop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(NOOP, _) => writeOk
  }

  /**
   * <pre>
   * RSET
   *
   *     Arguments: none
   *
   *     Restrictions:
   *         may only be given in the TRANSACTION state
   *
   *     Discussion:
   *         If any messages have been marked as deleted by the POP3
   *         server, they are unmarked.  The POP3 server then replies
   *         with a positive response.
   *
   *     Possible Responses:
   *          +OK
   *
   *     Examples:
   *         C: RSET
   *         S: +OK maildrop has 2 messages (320 octets)
   * </pre>
   */
  def handleRset: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(RSET, _) => mailbox.messages.foreach(_.undelete)
  }

  val msgidAndLinePattern = """([0-9]+) ([0-9]+)""".r

  /**
   * <pre>
   * TOP msg n
   *
   *  Arguments:
   *         a message-number (required) which may NOT refer to to a
   *         message marked as deleted, and a non-negative number
   *         of lines (required)
   *
   *  Restrictions:
   *          may only be given in the TRANSACTION state
   *
   *  Discussion:
   *          If the POP3 server issues a positive response, then the
   *          response given is multi-line.  After the initial +OK, the
   *          POP3 server sends the headers of the message, the blank
   *          line separating the headers from the body, and then the
   *          number of lines of the indicated message's body, being
   *          careful to byte-stuff the termination character (as with
   *          all multi-line responses).
   *
   *          Note that if the number of lines requested by the POP3
   *          client is greater than than the number of lines in the
   *          body, then the POP3 server sends the entire message.
   *
   *  Possible Responses:
   *          +OK top of message follows
   *          -ERR no such message
   *
   *  Examples:
   *          C: TOP 1 10
   *          S: +OK
   * </pre>
   */
  def handleTop: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(TOP, Some(msgidAndLinePattern(msgIdStr, nbLinesStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      val nbLines = Integer.parseInt(nbLinesStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          writeOk
          msg.writeTo(writer, Some(nbLines))
          write(EOT)
      }
    case Pop3Command(TOP, arg) =>
      writeErr("Invalid argument for " + TOP + " requires msgid and line numbers as numeric values; got: " + arg)
  }

  /**
   * <pre>
   * UIDL [msg]
   *
   *  Arguments:
   *          a message-number (optional), which, if present, may NOT
   *          refer to a message marked as deleted
   *
   *  Discussion:
   *
   *         If an argument was given and the POP3 server issues a positive
   *         response with a line containing information for that message.
   *         This line is called a "unique-id listing" for that message.
   *
   *         If no argument was given and the POP3 server issues a positive
   *         response, then the response given is multi-line.  After the
   *         initial +OK, for each message in the maildrop, the POP3 server
   *         responds with a line containing information for that message.
   *         This line is called a "unique-id listing" for that message.
   *
   *         In order to simplify parsing, all POP3 servers are required to
   *         use a certain format for unique-id listings.  A unique-id
   *         listing consists of the message-number of the message,
   *         followed by a single space and the unique-id of the message.
   *         No information follows the unique-id in the unique-id listing.
   *
   *         The unique-id of a message is an arbitrary server-determined
   *         string, consisting of one to 70 characters in the range 0x21
   *         to 0x7E, which uniquely identifies a message within a
   *         maildrop and which persists across sessions.  This
   *         persistence is required even if a session ends without
   *         entering the UPDATE state.  The server should never reuse an
   *         unique-id in a given maildrop, for as long as the entity
   *         using the unique-id exists.
   *
   *         Note that messages marked as deleted are not listed.
   *
   *         While it is generally preferable for server implementations
   *         to store arbitrarily assigned unique-ids in the maildrop,
   *         this specification is intended to permit unique-ids to be
   *         calculated as a hash of the message.  Clients should be able
   *         to handle a situation where two identical copies of a
   *         message in a maildrop have the same unique-id.
   *
   *   Possible Responses:
   *         +OK unique-id listing follows
   *         -ERR no such message
   *
   *   Examples:
   *         C: UIDL
   *         S: +OK
   *         S: 1 whqtswO00WBw418f9t5JxYwZ
   *         S: 2 QhdPYR:00WBw1Ph7x7
   *         S: .
   *          ...
   *         C: UIDL 2
   *         S: +OK 2 QhdPYR:00WBw1Ph7x7
   *          ...
   *         C: UIDL 3
   *         S: -ERR no such message, only 2 messages in maildrop
   * </pre>
   */
  def handleUidl: PartialFunction[Pop3Command, Unit] = {
    case Pop3Command(UIDL, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr("Message not found")
        case Some(msg) =>
          if (msg.isDeleted)
            writeErr("Message deleted.")
          else {
            writeOk(msgid + " " + msg.uniqueId)
          }
      }
    case Pop3Command(UIDL, _) =>
      writeOk
      var index = 1
      mailbox.messages.foreach((m: Message) => {
        writeOk(index + " " + m.uniqueId)
        index = index + 1
      })
      write(EOT)
  }
}
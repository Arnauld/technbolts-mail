package org.technbolts.mail.pop3

import org.slf4j.{Logger, LoggerFactory}
import org.technbolts.mail.{Message, Mailbox, Credentials}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  base class for session
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
trait Pop3Session[T] extends Pop3IO[T] {

  def dispose:Unit = {}

  def isClosed = false

  private abstract class Pop3SessionLogger
  val logger: Logger = LoggerFactory.getLogger(classOf[Pop3SessionLogger])

  def handle(e: T): Pop3Session[T] = handleCommand(e, getCommand(e))

  import Pop3Command._
  def handleCommand(e: T, command:Pop3Command): Pop3Session[T] = command match {
    case Pop3Command(QUIT, _) =>
      dispose
      newClosed(e)
    case Pop3Command(what, _) => doUnsupported(e, what)
  }

  def doUnsupported(e:T, what:String): Pop3Session[T] = {
    writeErr(e, "Unsupported command: " + what)
    throw new UnsupportedCommandException("Unsupported command: " + what)
  }

  def newClosed(e:T): Pop3Session[T]

  def getCommand(e:T): Pop3Command = {
    val line = readLine(e)
    lineToCommand(line)
  }

  val msgidPattern = """([0-9]+)""".r

  val msgidAndLinePattern = """([0-9]+) ([0-9]+)""".r
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Closed
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
trait Pop3SessionClosed[T] extends Pop3Session[T] {
  override def isClosed = true
  override def handle(e: T) = throw new IllegalStateException("Session is closed")
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Authorization
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
trait Pop3SessionAuthorization[T] extends Pop3Session[T] {
  def credentials: Credentials
  def serverContext: Pop3Server

  private abstract class Pop3SessionAuthorizationLogger
  override val logger: Logger = LoggerFactory.getLogger(classOf[Pop3SessionAuthorizationLogger])

  def newAuthorization(newCredentials: Credentials):Pop3SessionAuthorization[T]

  def newTransaction(mailbox: Mailbox):Pop3SessionTransaction[T]

  override def handleCommand(e: T, command: Pop3Command) =
    (handleUser(e) orElse handlePass(e) orElse handleDefault(e))(command)

  def handleDefault(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case command:Pop3Command =>
      super.handleCommand(e, command)
  }

  import Pop3Command._

  /*
   * TODO: handle the side effect of locking the mailbox
   * if one doesn't use the returned session, lock on mailbox should be discarded
   * => a new session is created AND a new server state should be created to allow
   *    an easy rollback...
   */
  def handlePass(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(PASS, None) =>
      writeErr(e, "No password provided")
      this
    case Pop3Command(PASS, Some(password)) =>
      val newCredentials = credentials.withPass(password)
      handleCredentialsComplete(e, newCredentials)
  }

  private def handleCredentialsComplete(e:T, newCredentials:Credentials):Pop3Session[T] =
    newCredentials.isSatisfied match {
      case true =>
        serverContext.lockAndGetMailbox(newCredentials) match {
          case None =>
            // failed to lock the mailbox, maybe alreday locked
            writeErr(e, "Mailbox already locked")
            newAuthorization(newCredentials)
          case Some(mbox) =>
            writeOk(e, "Login successful")
            newTransaction(mbox)
        }
      case false =>
        writeErr(e, "Invalid credentials")
        newAuthorization(newCredentials)
    }

  def handleUser(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(USER, None) =>
      writeErr(e, "No user provided")
      this
    case Pop3Command(USER, Some(login)) =>
      writeOk(e, "Password required for " + login)
      val newCredentials = credentials.withUser(login)
      newAuthorization(newCredentials)
  }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  Transaction
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
trait Pop3SessionTransaction[T] extends Pop3Session[T] {
  def mailbox:Mailbox
  def serverContext: Pop3Server

  private abstract class Pop3SessionTransactionLogger
  override val logger: Logger = LoggerFactory.getLogger(classOf[Pop3SessionTransactionLogger])

  override def dispose:Unit = {
    serverContext.unlockMailbox(mailbox)
  }

  def writer(e: T): (String) => Unit = (line: String) => writeLine(e, line)

  import Pop3Command._

  override def handleCommand(e: T, command: Pop3Command) =
    (handleStat(e)
            orElse handleTop(e)
            orElse handleRetr(e)
            orElse handleNoop(e)
            orElse handleList(e)
            orElse handleDele(e)
            orElse handleDefault(e))(getCommand(e))

  def handleDefault(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case command:Pop3Command =>
      super.handleCommand(e, command)
  }

  def handleRetr(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(RETR, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr(e, "Message not found")
        case Some(msg) =>
          writeOk(e)
          msg.writeTo(writer(e), None)
          writeLine(e, ".")
      }
      this
    case Pop3Command(RETR, arg) =>
      writeErr(e, "Invalid argument for " + RETR + " requires msgid as numeric values; got: " + arg)
      this
  }

  def handleStat(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(STAT, _) =>
      writeOk(e, mailbox.getNumberOfMessage + " " + mailbox.getSizeOfAllMessage)
      this
  }

  def handleNoop(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(NOOP, _) =>
      writeOk(e)
      this
  }

  def handleTop(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(TOP, Some(msgidAndLinePattern(msgIdStr, nbLinesStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      val nbLines = Integer.parseInt(nbLinesStr)
      mailbox.getMessage(msgid - 1) match {
        case None =>
          writeErr(e, "Message not found")
        case Some(msg) =>
          writeOk(e)
          msg.writeTo(writer(e), Some(nbLines))
          writeLine(e, ".")
      }
      this
    case Pop3Command(TOP, arg) =>
      writeErr(e, "Invalid argument for " + TOP + " requires msgid and line numbers as numeric values; got: " + arg)
      this
  }

  def handleList(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(LIST, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr(e, "Message not found")
        case Some(msg) =>
          if (msg.isMarkedAsDeleted)
            writeErr(e, "Message deleted.")
          else {
            writeOk(e, msgid + " " + msg.size)
          }
      }
      this
    case Pop3Command(LIST, _) =>
      writeOk(e, mailbox.getNumberOfMessage + " messages (" + mailbox.getSizeOfAllMessage + " octets) ")
      var index = 1
      mailbox.messages.foreach((m: Message) => {
        writeOk(e, index + " " + m.size)
        index = index + 1
      })
      writeLine(e, ".")
      this
  }

  /*
   * TODO: handle the side effect of deleting a message => a new mailbox instance should be returned with the modified message
   */
  def handleDele(e: T): PartialFunction[Pop3Command, Pop3Session[T]] = {
    case Pop3Command(DELE, Some(msgidPattern(msgIdStr))) =>
      val msgid = Integer.parseInt(msgIdStr)
      mailbox.getMessage(msgid - 1) match {
        case None => writeErr(e, "Message not found")
        case Some(msg) =>
          if (msg.isMarkedAsDeleted)
            writeErr(e, "Message already deleted.")
          else {
            msg.markAsDeleted
            writeOk(e)
          }
      }
      this
    case Pop3Command(DELE, arg) =>
      writeErr(e, "Invalid argument for " + DELE + " requires msgid as numeric values; got: " + arg)
      this
  }
}

package org.technbolts.mail.pop3

object Pop3Command {
  /**indicates it is not a pop3 command as defined in rfc ... */
  val NULL = "NULL"

  val QUIT = "QUIT"
  val USER = "USER"
  val PASS = "PASS"
  val STAT = "STAT"
  val LIST = "LIST"
  val RETR = "RETR"
  val DELE = "DELE"
  val NOOP = "NOOP"
  val RSET = "REST"
  val TOP = "TOP"
  val UIDL = "UIDL"

  implicit def lineToCommand(line: String): Pop3Command = line.indexOf(" ") match {
    case indexOf if indexOf > -1 => Pop3Command(line.substring(0, indexOf), Some(line.substring(indexOf + 1)))
    case _ => Pop3Command(line, None)
  }
}

case class Pop3Command(command: String, argument: Option[String])
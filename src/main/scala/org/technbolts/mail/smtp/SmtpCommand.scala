package org.technbolts.mail.smtp

object SmtpCommand {
  /** indicates it is not a smtp command as defined in rfc ... */
  val NULL = "NULL"

  val QUIT = "QUIT"
  val EHLO = "EHLO"
  val RSET = "RSET"
  val NOOP = "NOOP"
  val MAIL = "MAIL"
  val RCPT = "RCPT"
  val DATA = "DATA"
}

case class SmtpCommand(command: String, argument: Option[String])
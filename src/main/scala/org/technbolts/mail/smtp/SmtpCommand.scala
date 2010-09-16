package org.technbolts.mail.smtp

object SmtpCommand {
  /** indicates it is not a smtp command as defined in rfc ... */
  val NULL = "NULL"

  val QUIT = "QUIT"
  val EHLO = "EHLO"
  val HELO = "HELO"
  val RSET = "RSET"
  val NOOP = "NOOP"
  val MAIL = "MAIL FROM:"
  val RCPT = "RCPT TO:"
  val DATA = "DATA"
}

case class SmtpCommand(command: String, argument: Option[String])
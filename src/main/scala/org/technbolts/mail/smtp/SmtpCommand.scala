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

  implicit def stringToCommand(line:String): SmtpCommand = line match {
      // special case of the two words commands
      case x if line.startsWith(SmtpCommand.MAIL) =>
        SmtpCommand(SmtpCommand.MAIL, Some(line.substring(SmtpCommand.MAIL.length)))
      case x if line.startsWith(SmtpCommand.RCPT) =>
        SmtpCommand(SmtpCommand.RCPT, Some(line.substring(SmtpCommand.RCPT.length)))

      // otherwise use space to extract command and parameters
      case _ => line.indexOf(" ") match {
        case indexOf if indexOf > -1 =>
          SmtpCommand(line.substring(0, indexOf), Some(line.substring(indexOf + 1)))
        case _ =>
          SmtpCommand(line, None)
        }
  }
}

case class SmtpCommand(command: String, argument: Option[String])
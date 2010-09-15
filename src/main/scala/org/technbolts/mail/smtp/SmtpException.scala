package org.technbolts.mail.smtp

class SmtpException(message:String, cause:Throwable)  extends Exception(message,cause)

class QuitException extends SmtpException(null, null)

class UnsupportedCommandException(message: String) extends SmtpException(message, null)
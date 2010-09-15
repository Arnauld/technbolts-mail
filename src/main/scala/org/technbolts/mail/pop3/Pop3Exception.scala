package org.technbolts.mail.pop3

class Pop3Exception(message:String, cause:Throwable)  extends Exception(message,cause)

class QuitException extends Pop3Exception(null, null)

class UnsupportedCommandException(message: String) extends Pop3Exception(message, null)

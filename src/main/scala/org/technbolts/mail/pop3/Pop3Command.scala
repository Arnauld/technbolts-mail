package org.technbolts.mail.pop3

object Pop3Command {
  val QUIT = "QUIT";
  val USER = "USER";
  val PASS = "PASS";
  val STAT = "STAT";
  val LIST = "LIST";
  val RETR = "RETR";
  val DELE = "DELE";
  val NOOP = "NOOP";
  val RSET = "REST";
  val TOP = "TOP";
  val UIDL = "UIDL";
}

case class Pop3Command(command: String, argument: Option[String])
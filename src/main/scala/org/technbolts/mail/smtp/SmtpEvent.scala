package org.technbolts.mail.smtp

import java.net.Socket

sealed abstract class SmtpEvent(server: SmtpServer)
case class OnSmtpServerStart(server: SmtpServer) extends SmtpEvent(server)
case class OnSmtpServerStop(server: SmtpServer) extends SmtpEvent(server)
case class OnSmtpSessionPreInit(server: SmtpServer, socket: Socket) extends SmtpEvent(server)
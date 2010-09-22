package org.technbolts.mail.pop3

import java.net.Socket

sealed abstract class Pop3Event(server: Pop3Server)

case class OnPop3ServerStart(server: Pop3Server) extends Pop3Event(server)
case class OnPop3ServerStop(server: Pop3Server) extends Pop3Event(server)
case class OnPop3SessionAuth(server: Pop3Server, socket: Socket) extends Pop3Event(server)
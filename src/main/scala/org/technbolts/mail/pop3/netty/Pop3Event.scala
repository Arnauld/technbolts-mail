package org.technbolts.mail.pop3.netty

sealed trait Pop3Event
case class OnPop3ServerStart(server: Pop3Server) extends Pop3Event
case class OnPop3ServerStop (server: Pop3Server) extends Pop3Event
case class OnPop3SessionAuth[T](session: Pop3Session[T]) extends Pop3Event

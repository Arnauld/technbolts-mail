package org.technbolts.mail.pop3.netty

import org.slf4j.{LoggerFactory, Logger}

trait Pop3IO[T] {
  def logger: Logger = LoggerFactory.getLogger(classOf[Pop3IO])

  val CRLF = "\r\n"

  def readLine(io: T): String

  def writeLine(io: T, message: String): Unit

  def writeOk(io: T): Unit = {
    writeLine(io, "+OK")
    logger.debug("> +OK")
  }

  def writeOk(io: T, message: String): Unit = {
    writeLine(io, "+OK " + message)
    logger.debug("> +OK " + message)
  }

  def writeErr(io: T, message: String): Unit = {
    writeLine(io, "-ERR " + message)
    logger.debug("> -ERR " + message)
  }
}
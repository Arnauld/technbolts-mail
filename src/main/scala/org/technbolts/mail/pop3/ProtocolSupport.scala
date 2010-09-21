package org.technbolts.mail.pop3

import java.io.{BufferedWriter, PrintWriter, BufferedReader}

trait ProtocolSupport {
  def reader: BufferedReader
  def writer: BufferedWriter
  
  //
  val CRLF = "\r\n"

  // End Of Transmission
  val EOT = "."

  import org.slf4j.{Logger, LoggerFactory}
  private val logger: Logger = LoggerFactory.getLogger(classOf[ProtocolSupport])

  /**
   * Writes the specified output message to the client.
   */
  def writeErr(message: String): Unit = {
    logger.debug("Writing ERR: {}", message)
    writer.write("-ERR ")
    writer.write(message)
    writer.write(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def writeOk(message: String): Unit = {
    logger.debug("Writing OK: {}", message)
    writer.write("+OK ")
    writer.write(message)
    writer.write(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def writeOk(): Unit = {
    logger.debug("Writing OK")
    writer.write("+OK ")
    writer.write(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def write(message: String): Unit = {
    logger.debug("Writing Output: {}", message)
    writer.write(message)
    writer.write(CRLF);
    writer.flush();
  }

  /**
   * Reads a line from the input stream and returns it.
   */
  def readLine: String = {
    val line = reader.readLine match {
      case null => Pop3Command.NULL
      case read => read.trim
    }
    logger.debug("Reading Input: {}", line)
    line;
  }

  def readCommand: Pop3Command = {
    val line = readLine
    val command = Pop3Command.lineToCommand(line)
    logger.info("Received: {}", command)
    command
  }
}
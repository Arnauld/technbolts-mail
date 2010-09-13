package org.technbolts.mail.pop3

import java.io.{PrintWriter, BufferedReader}

trait ProtocolSupport {
  def reader: BufferedReader
  def writer: PrintWriter
  
  //
  val CRLF = "\r\n"

  import org.slf4j.{Logger, LoggerFactory}
  private val logger: Logger = LoggerFactory.getLogger(classOf[ProtocolSupport])

  /**
   * Writes the specified output message to the client.
   */
  def writeErr(message: String): Unit = {
    logger.debug("Writing ERR: {}", message)
    writer.print("-ERR ")
    writer.print(message)
    writer.print(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def writeOk(message: String): Unit = {
    logger.debug("Writing OK: {}", message)
    writer.print("+OK ")
    writer.print(message)
    writer.print(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def writeOk(): Unit = {
    logger.debug("Writing OK")
    writer.print("+OK ")
    writer.print(CRLF);
    writer.flush();
  }

  /**
   * Writes the specified output message to the client.
   */
  def write(message: String): Unit = {
    logger.debug("Writing Output: {}", message)
    writer.print(message)
    writer.print(CRLF);
    writer.flush();
  }

  /**
   * Reads a line from the input stream and returns it.
   */
  def readLine: String = {
    val line = reader.readLine().trim();
    logger.debug("Reading Input: {}", line)
    line;
  }

  def readCommand: Pop3Command = {
    val line = readLine
    val indexOf = line.indexOf(" ")
    if (indexOf > -1)
      Pop3Command(line.substring(0, indexOf), Some(line.substring(indexOf + 1)))
    else
      Pop3Command(line, None)
  }
}
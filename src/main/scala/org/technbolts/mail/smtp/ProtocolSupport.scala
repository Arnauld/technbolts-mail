package org.technbolts.mail.smtp

import java.io.{BufferedWriter, BufferedReader}

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
  def write(message: String): Unit = {
    logger.debug("Writing Output: {}", message)
    writer.write(message)
    writer.write(CRLF)
    writer.flush()
  }

  def writeBadSequence:Unit = write("503 Bad sequence of commands")
  
  def writeOk:Unit = write("250 OK")

  /**
   * Reads a line from the input stream and returns it.
   */
  def readLine: String = {
    val line = reader.readLine match {
      case null => SmtpCommand.NULL
      case read => read.trim
    }
    logger.debug("Reading Input: {}", line)
    line
  }

  def readCommand: SmtpCommand = {
    val line = readLine
    val command =
      // special case of the two words commands
      if(line.startsWith(SmtpCommand.MAIL)) {
        SmtpCommand(SmtpCommand.MAIL, Some(line.substring(SmtpCommand.MAIL.length)))
      }
      else if(line.startsWith(SmtpCommand.RCPT)) {
        SmtpCommand(SmtpCommand.RCPT, Some(line.substring(SmtpCommand.RCPT.length)))
      }
      else line.indexOf(" ") match {
        case indexOf if indexOf > -1 =>
          SmtpCommand(line.substring(0, indexOf), Some(line.substring(indexOf + 1)))
        case _ =>
          SmtpCommand(line, None)
      }
    logger.info("Received: {}", command)
    command
  }
}
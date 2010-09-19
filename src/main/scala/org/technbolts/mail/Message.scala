package org.technbolts.mail

import org.slf4j.{Logger, LoggerFactory}
import java.io._
import collection.mutable.{ListBuffer, HashSet}
import org.apache.commons.io.IOUtils
import org.technbolts.util.EventDispatcher

class Message(val file:File) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Message])

  val CRLF = "\r\n"
  var encoding = "iso-8859-1"

  def size = if(file.exists) file.length else 0L
  def timestamp = if(file.exists) file.lastModified else 0L

  private var deleteMark = false
  def markAsDeleted:Unit = deleteMark=true
  def unmarkAsDeleted:Unit = deleteMark=false
  def isMarkedAsDeleted = deleteMark

  def delete:Unit = file.exists match {
      case true => file.delete match {
        case false => logger.warn("Failed to delete <" + file.getAbsolutePath + ">")
        case true => logger.debug("File <" + file.getAbsolutePath + "> deleted!")
      }
      case false => logger.warn("Nothing to delete <" + file.getAbsolutePath + ">")
  }

  def uniqueId:String = {
    val msgid = "message-id: "
    headers.find( _.toLowerCase.startsWith(msgid) ) match {
      case None => file.getName
      case Some(line) => line.substring(msgid.length)
    }
  }

  def headers:List[String] = {
    val lines = new ListBuffer[String] ()
    val writer = (s:String)=> { lines.append(s) }
    writeTo(writer, Some(0))
    lines.toList
  }

  def writeStream(append:Boolean) = new FileOutputStream(file, append)

  def readStream = new FileInputStream(file)

  def writeTo(writer:Writer, nbLines:Option[Int]):Unit = {
    val write = (s:String)=> {
      writer.write(s)
      writer.write(CRLF)
    }
    writeTo(write, nbLines)
  }

  def writeTo(writer:(String)=>Unit, nbLines:Option[Int]):Unit = {
    val inputStream: FileInputStream = new FileInputStream(file)
    try {
      val content = new BufferedReader(new InputStreamReader(inputStream, encoding))

      // initial potential empty lines
      var line: String = content.readLine
      while (line != null && line.length == 0) {
        writer(line)
        line = content.readLine
      }

      // write headers
      while (line != null && line.length > 0) {
        writer(line)
        line = content.readLine
      }

      // empty lines separating header from body
      while (line != null && line.length == 0) {
        writer(line)
        line = content.readLine
      }

      // write the TOP nbLines of the body
      var remaining = nbLines.getOrElse(Integer.MAX_VALUE)
      while (line != null && remaining > 0) {
        writer(line)
        remaining = remaining - 1
        line = content.readLine
      }
    }
    finally{
      IOUtils.closeQuietly(inputStream)
    }
  }

}









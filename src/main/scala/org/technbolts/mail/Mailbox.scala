package org.technbolts.mail

import org.slf4j.{Logger, LoggerFactory}
import java.io._
import collection.mutable.{ListBuffer, HashSet}

class Message(val file:File) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Message])

  val CRLF = "\r\n"
  var encoding = "iso-8859-1"

  def size = if(file.exists) file.length else 0L
  def timestamp = if(file.exists) file.lastModified else 0L

  private var deleted = false
  def delete:Unit = deleted=true
  def undelete:Unit = deleted=false
  def isDeleted = deleted

  def deleteFile:Unit = {
    if(file.exists)
      file.delete
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

  def writeTo(writer:Writer, nbLines:Option[Int]):Unit = {
    val write = (s:String)=> {
      writer.write(s)
      writer.write(CRLF)
    }
    writeTo(write, nbLines)
  }

  def writeTo(writer:(String)=>Unit, nbLines:Option[Int]):Unit = {
    val content = new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))

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
}

object MailboxRepository {
  def get:MailboxRepository = {
    new MailboxRepository(new File("./target/"))
  }
}

class MailboxRepository(rootDir:File) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[MailboxRepository])

  private val locked = new HashSet[String]
  def acquireMailbox(user:User)(pf:PartialFunction[Option[Mailbox],Unit]):Unit = {
    try{
      pf( synchronized {
            locked.add(user.login) match {
              case true =>
                logger.info("Mailbox <{}> locked", user.login)
                Some(new Mailbox (user, rootDir))
              case false =>
                logger.warn("Mailbox <{}> already locked", user.login)
                None
            }
          } )
    }
    finally{
      synchronized {
        logger.info("Mailbox <{}> unlocked", user.login)
        locked.remove(user.login)
      }
    }
  }
}

class Mailbox(user:User, rootDir:File) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Mailbox])

  val mailboxDir = new File(rootDir, user.login)
  if(!mailboxDir.exists)
    mailboxDir.mkdirs
  
  logger.info("Mailbox for user <{}> located at: {}", user.login, mailboxDir.getAbsolutePath)

  def getNumberOfMessage:Int = messages.size

  def getSizeOfAllMessage:Long = messages.foldLeft(0L)((acc,msg) => acc+msg.size)

  lazy val messages:Array[Message] = loadMessages
  private def loadMessages = {
    val files = mailboxDir.listFiles(new FileFilter {
      def accept(file: File) = !file.isDirectory && file.getName.toLowerCase.endsWith(".eml")
    })
    val msgs = files.map( (f:File) => {new Message(f)})
    msgs.sortWith((msg1:Message,msg2:Message) => msg1.timestamp < msg2.timestamp)
  }

  def getMessage(idx:Int):Option[Message] = {
    val msgs = messages
    idx match {
      case x if(x >= 0 && x < msgs.size) => Some(msgs(idx))
      case _ => None
    }
  }

  def processDeleted:Unit = messages.filter( _.isDeleted ).foreach( _.deleteFile )
}

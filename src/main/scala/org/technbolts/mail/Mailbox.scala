package org.technbolts.mail

import java.io.{FileFilter, File}
import org.slf4j.{Logger, LoggerFactory}

class Message(val file:File) {
  def size = if(file.exists) file.length else 0L
  def timestamp = if(file.exists) file.lastModified else 0L
}

class Mailbox(user:User, rootDir:File) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Mailbox])

  val mailboxDir = new File(rootDir, user.login)
  if(!mailboxDir.exists)
    mailboxDir.mkdirs
  logger.info("Mailbox for user " + user.login +" opened in: " + mailboxDir.getAbsolutePath)

  def getNumberOfMessage:Int = messages.size

  def getSizeOfAllMessage:Long = messages.foldLeft(0L)((acc,msg) => acc+msg.size)

  private def messages:Array[Message] = mailboxDir.listFiles(new FileFilter {
    def accept(file: File) = !file.isDirectory && file.getName.toLowerCase.endsWith(".eml")
  }).map( (f:File) => {new Message(f)})

  private def orderedMessages:Array[Message] =
    messages.sortWith((msg1:Message,msg2:Message) => msg1.timestamp < msg2.timestamp)

  def getMessage(idx:Int):Option[Message] = {
    val msgs = orderedMessages
    if(idx<msgs.size)
      Some(msgs(idx))
    else
      None
  }
}

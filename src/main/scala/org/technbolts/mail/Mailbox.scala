package org.technbolts.mail

import org.slf4j.{Logger, LoggerFactory}
import java.io._
import collection.mutable.HashSet
import org.technbolts.util.EventDispatcher
import java.util.concurrent.atomic.AtomicInteger
import java.text.SimpleDateFormat
import java.util.Date


object MailboxRepository {
  def get:MailboxRepository = {
    new MailboxRepository(new File("./target/"))
  }
}

sealed abstract class MailboxEvent(mailbox: Mailbox)
case class OnMailboxLoaded(mailbox: Mailbox) extends MailboxEvent(mailbox)

class MailboxRepository(rootDir:File) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[MailboxRepository])

  val listeners = new EventDispatcher[MailboxEvent]

  val temporaryMessageDir = new File(rootDir, "temporary")
  if(!temporaryMessageDir.exists)
    temporaryMessageDir.mkdirs
  logger.info("Temporary messages saved in {}", temporaryMessageDir.getAbsolutePath)

  private val locked = new HashSet[String]
  def acquireMailbox(user:User)(pf:PartialFunction[Option[Mailbox],Unit]):Unit = {
    try{
      pf( lockAndGetMailbox(user) )
    }
    finally{
      synchronized {
        logger.info("Mailbox <{}> unlocked", user.login)
        locked.remove(user.login)
      }
    }
  }

  def lockAndGetMailbox(user:User):Option[Mailbox] = {
    synchronized { locked.add(user.login) } match {
      case true =>
        logger.info("Mailbox <{}> locked", user.login)

        val mailboxDir = new File(rootDir, user.login)
        val mbox = new Mailbox (user, mailboxDir)
        listeners.publishEvent(OnMailboxLoaded(mbox))
        Some(mbox)
      case false =>
        logger.warn("Mailbox <{}> already locked", user.login)
        None
    }
  }

  private val idCounter = new AtomicInteger
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd~HH-mm-ss")

  def temporaryMessage:Message = {
    val fileName = "t" + dateFormat.format(new Date) + "~" + idCounter.incrementAndGet + ".eml"
    val msgFile = new File(temporaryMessageDir, fileName)
    new Message(msgFile)
  }

}

class Mailbox(user:User, mailboxDir:File) {
  var doDelete:(Message)=>Unit = { _.delete }

  private val logger: Logger = LoggerFactory.getLogger(classOf[Mailbox])

  logger.info("Mailbox for user <{}> located at: {}", user.login, mailboxDir.getAbsolutePath)
  if(!mailboxDir.exists)
    mailboxDir.mkdirs
  
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

  def processDeleted:Unit = messages.filter( _.isMarkedAsDeleted ).foreach( (m)=> doDelete(m) )
}

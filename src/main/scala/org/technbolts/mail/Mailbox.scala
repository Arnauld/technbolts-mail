package org.technbolts.mail

import java.io.{FileFilter, File}

class Mailbox(user:User, rootDir:File) {

  val mailboxDir = new File(rootDir, user.login)
  if(!mailboxDir.exists)
    mailboxDir.mkdirs

  def getNumberOfMessage:Int = messages.size

  def getSizeOfAllMessage:Long = messages.foldLeft(0L)((acc,file) => acc+file.length)

  private def messages:Array[File] = mailboxDir.listFiles(new FileFilter {
    def accept(file: File) = !file.isDirectory && file.getName.toLowerCase.endsWith(".eml")
  })

  private def orderedMessages:Array[File] = messages.sortWith((file1:File,file2:File) => file1.lastModified < file2.lastModified)

  def getMessage(idx:Int):Option[File] = {
    val msgs = orderedMessages
    if(idx<msgs.size)
      Some(msgs(idx))
    else
      None
  }
}

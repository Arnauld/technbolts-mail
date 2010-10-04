package runner

import org.technbolts.mail.pop3.Pop3Server
import org.technbolts.util.LangUtils
import io.Source
import org.technbolts.mail.pop3.netty.Pop3NettyServer
import org.technbolts.mail.{MailboxRepository, OnMailboxLoaded}
import java.io.File

object Pop3ServerRunner {
  def main(args: Array[String]):Unit = {
    /*
     * port
     */
    System.out.print("POP3 port [111]: ")
    val portStr = Source.fromInputStream(System.in).getLines.next
    val port = if(LangUtils.isEmpty(portStr)) 111 else Integer.parseInt(portStr)

    /*
     * mailboxes directory
     */
    val userHome = System.getProperty("user.home")
    var rootDir = new File(userHome)
    System.out.print("Mailboxes root directory [" + rootDir.getAbsolutePath + "]: ")
    val rootDirStr = Source.fromInputStream(System.in).getLines.next
    if(!LangUtils.isEmpty(rootDirStr))
      rootDir = new File(rootDirStr)

    val mailBoxRepository = new MailboxRepository(rootDir)

    /*
     * keep message on markDeleted
     */
    System.out.print("POP3 keep mail on delete (y/n) [y]: ")
    val keepStr = Source.fromInputStream(System.in).getLines.next
    if(LangUtils.isEmpty(keepStr) || keepStr.equalsIgnoreCase("y")) {
      mailBoxRepository.listeners++{
        case OnMailboxLoaded(mailbox) => mailbox.doDelete = (m)=>{}
      }
    }

    /*
     * start!
     */
    val server:Pop3Server = new Pop3NettyServer(port, mailBoxRepository)
    server.start
  }
}

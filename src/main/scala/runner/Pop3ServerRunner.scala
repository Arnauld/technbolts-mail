package runner

import org.technbolts.mail.pop3.Pop3Server
import org.technbolts.util.LangUtils
import io.Source
import org.technbolts.mail.{OnMailboxLoaded}

object Pop3ServerRunner {
  def main(args: Array[String]):Unit = {
    /*
     * port
     */
    System.out.print("POP3 port [111]: ")
    val portStr = Source.fromInputStream(System.in).getLines.next
    val port = if(LangUtils.isEmpty(portStr)) 111 else Integer.parseInt(portStr)

    val server:Pop3Server = Pop3Server(port)

    /*
     * keep message on markDeleted
     */
    System.out.print("POP3 keep mail on delete (y/n) [y]: ")
    val keepStr = Source.fromInputStream(System.in).getLines.next
    if(LangUtils.isEmpty(keepStr) || keepStr.equalsIgnoreCase("y")) {
      server.mailboxRepository.listeners++{
        case OnMailboxLoaded(mailbox) => mailbox.doDelete = (m)=>{}
      }
    }
    server.start
  }
}

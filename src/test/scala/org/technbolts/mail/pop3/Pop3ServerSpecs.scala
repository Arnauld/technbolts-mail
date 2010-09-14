package org.technbolts.mail.pop3

import org.specs.Specification
import java.io._
import org.mockito.Mockito
import org.technbolts.mail.{MailboxRepository, Mailbox, User}

class Pop3ServerSpecs extends Specification {
}

class Pop3SessionSpecs extends Specification {

  "Pop3Session" should {
    "handle TOP command" in {
      val reader = new BufferedReader(new StringReader("TOP 0 1"))
      val writer = new StringWriter

      val mboxRepository = Mockito.mock(classOf[MailboxRepository])
      val state  = new Pop3ServerState(mboxRepository)
      val handler = new Pop3Session("<pid>", state, reader, new BufferedWriter(writer), ()=>{})
      handler.mailbox = new Mailbox(new User("login","pass"), new File("target/mbox"))
      handler.handleTop(Pop3Command("TOP", Some("1 1")))
      println (writer.toString)
    }
  }
}
package org.technbolts.mail.pop3

import org.specs.Specification
import org.technbolts.mail.{Mailbox, User}
import java.io._

class Pop3ServerSpecs extends Specification {
}

class Pop3HandlerSpecs extends Specification {

  "Pop3Session" should {
    "handle TOP command" in {
      val reader = new BufferedReader(new StringReader("TOP 0 1"))
      val writer = new StringWriter
      val handler = new Pop3Session("<pid>", reader, new PrintWriter(writer), { case Pop3Command("WEIRD", _) => }, ()=>{})
      handler.mailbox = new Mailbox(new User("login","pass"), new File("target/mbox"))
      handler.handleTop(Pop3Command("TOP", Some("0 1")))
      println (writer.toString)
    }
  }
}
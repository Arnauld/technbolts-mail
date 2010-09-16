package org.technbolts.mail.pop3

import org.specs.Specification
import java.io._
import org.mockito.Mockito
import java.util.Properties
import javax.mail.{Message, Folder, Session}
import org.technbolts.mail.{MailboxListener, MailboxRepository, Mailbox, User}
import java.util.concurrent.CyclicBarrier
import collection.JavaConversions

object Env {
  def startServer(port:Int):Pop3Server = {
    val server = Pop3Server(port, new File("E:\\Arnauld\\mailboxes"))
    server.mailboxRepository.listeners.append( new MailboxListener {
      override def onCreation(mbox:Mailbox):Unit = mbox.ignoreDelete = true
    })
    val barrier = new CyclicBarrier (2)
    server.listeners.append(new Pop3ServerListener {
      override def onStart(server:Pop3Server):Unit = barrier.await
    })

    // start server in a separate thread
    new Thread(new Runnable {
      override def run:Unit = server.start
    }).start

    // wait the server is started
    barrier.await
    server
  }
}

class Pop3ServerSpecs extends Specification {

  "Pop3Server" should {
    "handle new connection from javamail" in {
      val port = 111

      val server = Env.startServer (port)

      val props = new Properties
      val session = Session.getDefaultInstance(props, null)
      val store = session.getStore("pop3")
      store.connect("127.0.0.1", port, "user@Technbolts", "p4ss0rd")

      val folder = store.getFolder("INBOX")
      folder.open(Folder.READ_ONLY);
      val messages = folder.getMessages
      var i = 1
      messages.foreach( (m:Message) => {
        val bous = new ByteArrayOutputStream
        m.writeTo(bous)
        println(i + ": " + m.getAllRecipients + "\t" + m.getSubject())
        JavaConversions.asIterator(m.getAllHeaders).foreach( println _ )
        i = i + 1
      })

      // Close connection
      folder.close(false)
      store.close()
      server.stop
    }
  }
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
      //println (writer.toString)
    }
  }
}
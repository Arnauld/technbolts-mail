package org.technbolts.mail.pop3.netty

import org.specs.Specification
import java.io._
import java.util.Properties
import collection.JavaConversions
import org.technbolts.TestSettings
import javax.mail.{Header, Message, Folder, Session}
import org.technbolts.mail.MailboxRepository

object Env {
  def settings = TestSettings()

  def startServer(port: Int): Pop3NettyServer = {
    val server = new Pop3NettyServer(port, new MailboxRepository(settings.workingDir))
    server.start
    server
  }
}

class Pop3ServerSpecs extends Specification {

  val port = 111
  var server:Pop3NettyServer = _

  "Pop3Server" should {
    doFirst {
      server = Env.startServer(port)
    }
    "handle new connection from javamail" in {
      val props = new Properties
      val session = Session.getDefaultInstance(props, null)
      val store = session.getStore("pop3")
      store.connect("127.0.0.1", port, "user@Technbolts", "p4ss0rd")

      val folder = store.getFolder("INBOX")
      folder.open(Folder.READ_ONLY);
      val messages = folder.getMessages
      var i = 1
      messages.foreach((m: Message) => {
        val bous = new ByteArrayOutputStream
        m.writeTo(bous)
        println(i + ": " + m.getAllRecipients + "\t" + m.getSubject())

        val writeHeaders = false
        if (writeHeaders) {
          JavaConversions.asIterator(m.getAllHeaders).foreach((header: Any) => {
            val h = header.asInstanceOf[Header]
            println(h.getName + "->" + h.getValue + ", ")
          })
        }
        i = i + 1
      })

      // Close connection
      folder.close(false)
      store.close()
      server.stop
    }
    doLast {
      server.stop
    }
  }
}

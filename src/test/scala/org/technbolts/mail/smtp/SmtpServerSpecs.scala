package org.technbolts.mail.smtp

import org.specs.Specification
import java.io._
import org.mockito.Mockito
import java.util.Properties
import javax.mail.{Message, Folder, Session}
import org.technbolts.mail.{MailboxListener, MailboxRepository, Mailbox, User}
import java.util.concurrent.CyclicBarrier
import collection.JavaConversions
import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSenderImpl}

object Env {
  def startServer(port: Int): SmtpServer = {
    val server = SmtpServer(port, new File("E:\\Arnauld\\mailboxes"))
    server.mailboxRepository.listeners.append(new MailboxListener {
      override def onCreation(mbox: Mailbox): Unit = mbox.ignoreDelete = true
    })
    val barrier = new CyclicBarrier(2)
    server.listeners.append(new SmtpServerListener {
      override def onStart(server: SmtpServer): Unit = barrier.await
    })

    // start server in a separate thread
    new Thread(new Runnable {
      override def run: Unit = server.start
    }).start

    // wait the server is started
    barrier.await
    server
  }
}

class SmtpServerSpecs extends Specification {
  "SmtpServer" should {
    "handle new connection from javamail" in {
      val port = 26
      val server = Env.startServer(port)

      val sender = new JavaMailSenderImpl
      sender.setHost("127.0.0.1")
      sender.setPort(port)

      val message = sender.createMimeMessage
      val helper = new MimeMessageHelper(message)
      helper.setTo("test@host.com")
      helper.setText("Thank you for ordering!")

      sender.send(message)
      server.stop
    }
  }
}
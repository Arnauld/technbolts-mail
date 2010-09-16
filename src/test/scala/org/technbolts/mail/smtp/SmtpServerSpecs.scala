package org.technbolts.mail.smtp

import org.specs.Specification
import java.io._
import org.mockito.Mockito
import java.util.Properties
import javax.mail.{Message, Folder, Session}
import java.util.concurrent.CyclicBarrier
import collection.JavaConversions
import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSenderImpl}
import org.technbolts.TestSettings
import org.technbolts.mail.{OnMailboxLoaded, MailboxRepository, Mailbox, User}

object Env {
  def settings = TestSettings()

  def startServer(port: Int): SmtpServer = {
    val server = SmtpServer(port, settings.workingDir)
    server.mailboxRepository.listeners++ {
      case OnMailboxLoaded(mbox: Mailbox) => mbox.doDelete = (m)=> {}
    }
    val barrier = new CyclicBarrier(2)
    server.listeners ++ {
      case OnSmtpServerStart(server) =>  barrier.await
    }

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
  val port = 26
  var server:SmtpServer = _

  "SmtpServer" should {
    doFirst {
      server = Env.startServer(port)
    }
    "handle new connection from javamail" in {
      val sender = new JavaMailSenderImpl
      sender.setHost("127.0.0.1")
      sender.setPort(port)

      val message = sender.createMimeMessage
      val helper = new MimeMessageHelper(message)
      helper.setTo("test@host.com")
      helper.setText("Thank you for ordering!")
      sender.send(message)
    }
    doLast {
      server.stop
    }
  }
}

class SmtpSessionSpecs extends Specification {
  "Mail pattern matching" should {
    "handle basic input" in {
      val found = "<yeah@Technbolts>" match {
        case SmtpSession.recipientPattern(mail,extra) => mail
        case _ => "<no-match>"
      } 
      found must_== "yeah@Technbolts"
    }
    "handle input with arguments" in {
      val (ef,ex) = "<yeah@Technbolts> bob" match {
        case SmtpSession.recipientPattern(mail,extra) => (mail,extra)
        case _ => "<no-match>"
      }
      ef must_== "yeah@Technbolts"
      ex must_== "bob"
    }
  }
}
package org.technbolts.mail.pop3.netty

import org.technbolts.mail._

trait Pop3Server {
  def mailboxRepository: MailboxRepository

  def lockAndGetMailbox(credentials: Credentials) =
    mailboxRepository.lockAndGetMailbox(User(credentials.userName.get, credentials.userPass.get))
}

package org.technbolts.mail.pop3

import org.technbolts.mail._

trait Pop3Server {

  def start:Unit

  def stop:Unit

  def mailboxRepository: MailboxRepository

  def lockAndGetMailbox(credentials: Credentials) =
    mailboxRepository.lockAndGetMailbox(User(credentials.userName.get, credentials.userPass.get))

  def unlockMailbox(mailbox:Mailbox) =
    mailboxRepository.unlockMailbox(mailbox)
}

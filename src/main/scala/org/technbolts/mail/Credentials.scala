package org.technbolts.mail

case class Credentials(userName: Option[String], userPass: Option[String]) {
  def withUser(user: String) = Credentials(Some(user), userPass)

  def withPass(pass: String) = Credentials(userName, Some(pass))

  def isSatisfied: Boolean = userName.isDefined && userPass.isDefined
}

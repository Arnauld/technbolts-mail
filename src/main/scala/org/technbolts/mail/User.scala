package org.technbolts.mail

import org.technbolts.util.LangUtils

object User {
  def apply(login:String, password:String) = new User(login, password)
}

class User(var login:String, var password:String) {
  import LangUtils._
  def isComplete:Boolean = isNotEmpty(login) && isNotEmpty(password)
  override def toString:String = "User["+login+"]"
}
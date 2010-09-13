package org.technbolts.util

object LangUtils {

  def isNotEmpty(string:String) = !isEmpty(string)
  def isEmpty(string:String) = (string==null || string.isEmpty)
}
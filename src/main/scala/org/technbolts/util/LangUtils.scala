package org.technbolts.util

object LangUtils {

  def isNotEmpty(string:String) = !isEmpty(string)
  def isEmpty(string:String) = (string==null || string.isEmpty)

  def combine[R,T](first:PartialFunction[R,T], others:List[PartialFunction[R,T]]):PartialFunction[R,T] = {
    others.foldLeft(first)((acc, x) => acc.orElse(x))
  }
}
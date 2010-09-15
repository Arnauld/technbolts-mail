package org.technbolts.util

object LangUtils {

  def isNotEmpty(string:String) = !isEmpty(string)
  def isEmpty(string:String) = (string==null || string.isEmpty)

  def combine[R,T](first:PartialFunction[R,T], others:List[PartialFunction[R,T]]):PartialFunction[R,T] = {
    others.foldLeft(first)((acc, x) => acc.orElse(x))
  }

  class CombinationPF[R,T](first:PartialFunction[R,T]) {
    var combination = first
    def orElse(others:List[PartialFunction[R,T]]):CombinationPF[R,T] = {
      combination = others.foldLeft(combination)((acc, x) => acc.orElse(x))
      this
    }
    def orElse(others:PartialFunction[R,T]*):CombinationPF[R,T] = {
      combination = others.foldLeft(combination)((acc, x) => acc.orElse(x))
      this
    }
    def get = combination
  }

  def combine[R,T](first:PartialFunction[R,T]):CombinationPF[R,T] = new CombinationPF(first)
}
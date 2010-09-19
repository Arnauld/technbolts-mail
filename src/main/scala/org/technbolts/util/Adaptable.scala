package org.technbolts.util

trait Adaptable {
  def getAdapter[T](klazz: Class[T]): Option[T]
}
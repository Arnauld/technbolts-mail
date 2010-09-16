package org.technbolts.util

import collection.mutable.ListBuffer

class EventDispatcher[T] {
  val listeners = new ListBuffer[PartialFunction[T,Unit]]
  val noopPf:PartialFunction[T,Unit] = { case _ => }

  def publishEvent(event: T): Unit = listeners.foreach( (pf) => pf(event) )

  def ++(listener:PartialFunction[T,Unit]):EventDispatcher[T] = {
    listeners.append(listener orElse noopPf)
    this
  }
}

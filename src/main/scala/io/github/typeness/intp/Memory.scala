package io.github.typeness.intp

import scala.collection.mutable

class Memory {
  private var stack: List[mutable.Map[String, Any]] = List(mutable.Map.empty)

  def pushNewStack(): Unit = {
    stack = stack.head.clone() :: stack
  }

  def define(obj: (String, Any)): Unit = {
    stack.head.put(obj._1, obj._2)
    ()
  }

  def popStack(): Unit = {
    stack = stack.tail
  }

  def get(name: String): Option[Any] = stack.head.get(name)

  def getAll: Map[String, Any] = stack.head.toMap

}

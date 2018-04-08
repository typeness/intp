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

  def pushNewLocalScope(): Unit = {
    pushNewStack()
  }

  def popNewLocalScope(): Unit = {
    val top = stack.head
    stack = stack.tail
    // Update modified variables
    val head = stack.head
    stack = stack.tail
    stack = head.map {
      case (name, value) => name -> top.getOrElse(name, value)
    } :: stack
    if (top.contains("return")) stack.head.put("return", top("return"))
  }


  def get(name: String): Option[Any] = stack.head.get(name)

  def getAll: Map[String, Any] = stack.head.toMap

}

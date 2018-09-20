package io.github.typeness.intp

import scala.collection.mutable

case class ObjectInMemory(name: String, value: TopType, scopeLevel: Int)

class Memory {
  private var stack: List[mutable.Map[String, ObjectInMemory]] = List(mutable.Map.empty)

  def scopeLevel: Int = stack.size

  def pushNewStack(): Unit = {
    stack = stack.head.clone() :: stack
  }

  def define(obj: (String, TopType)): Unit = {
    stack.head.put(obj._1, ObjectInMemory(obj._1, obj._2, scopeLevel))
    ()
  }

  def popStack(): Unit = {
    stack = stack.tail
  }

  def pushNewLocalScope(): Unit = pushNewStack()

  def popNewLocalScope(): Unit = {
    val top = stack.head
    stack = stack.tail
    // Update modified variables
    val head = stack.head
    stack = stack.tail
    stack = head.map {
      case (name, value) => name -> top.getOrElse(name, value)
    } :: stack
    if (top.contains("return")) {
      val _ = stack.head.put("return", top("return"))
    }
    if (top.get("break").map(_.value).contains(BooleanType(true))) {
      val _ = stack.head.put("break", top("break"))
    }
  }

  def get(name: String): Option[ObjectInMemory] = stack.head.get(name)

  def getAll: Map[String, ObjectInMemory] = stack.head.toMap


}

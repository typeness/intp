package io.github.typeness.intp

import scala.collection.mutable

case class ObjectInMemory(name: String, value: TopType, scopeLevel: Int, isMutable: Boolean)

class Memory {
  private var stack: List[mutable.Map[String, ObjectInMemory]] = List(mutable.Map.empty)

  def scopeLevel: Int = stack.size

  def pushNewStack(): Unit = {
    stack = stack.head.clone() :: stack
  }

  def assign(obj: (String, TopType)): Unit = {
    get(obj._1)
      .map(_.copy(value = obj._2))
      .map(newObj => stack.head.put(newObj.name, newObj))
    ()
  }

  def define(obj: (String, TopType), isMutable: Boolean): Unit = {
    stack.head.put(obj._1, ObjectInMemory(obj._1, obj._2, scopeLevel, isMutable))
    ()
  }

  private def updateVariables(
    old: mutable.Map[String, ObjectInMemory],
    updated: mutable.Map[String, ObjectInMemory]
  ): mutable.Map[String, ObjectInMemory] =
    old.map {
      case (name, value) =>
        updated.get(name) match {
          case Some(newObj) if newObj.scopeLevel == value.scopeLevel => name -> newObj
          case _                                                     => name -> value
        }
    }

  def popStack(): Unit = {
    val top = stack.head
    stack = stack.tail
    val head = stack.head
    stack = updateVariables(head, top) :: stack.tail
  }

  def pushNewLocalScope(): Unit = pushNewStack()

  def popNewLocalScope(): Unit = {
    val top = stack.head
    stack = stack.tail
    val head = stack.head
    stack = stack.tail
    stack = updateVariables(head, top) :: stack
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

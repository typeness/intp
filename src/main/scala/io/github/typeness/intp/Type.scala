package io.github.typeness.intp

import scala.collection.mutable

sealed trait TopType

case class IntegerType(value: Int) extends TopType {
  override def toString: String = s"$value"
}

case class ArrayType[T <: TopType](value: mutable.ArrayBuffer[T]) extends TopType {
  override def toString: String = Builtin.show(this)
}

case class FunctionType(value: FunctionLiteral) extends TopType {
  override def toString: String = "Function"
}

case class DoubleType(value: Double) extends TopType {
  override def toString: String = s"$value"
}

case class CharType(value: Char) extends TopType {
  override def toString: String = s"$value"
}

case class BooleanType(value: Boolean) extends TopType {
  override def toString: String = s"$value"
}

case class ObjectType(value: mutable.Map[String, TopType]) extends TopType {
  override def toString: String = Builtin.show(this)
}

case object UnitType extends TopType {
  override def toString: String = "Unit"
}

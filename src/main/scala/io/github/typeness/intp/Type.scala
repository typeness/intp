package io.github.typeness.intp

import scala.collection.mutable

object Type {
  def toEnum(top: TopType): Enum = top match {
    case IntegerType(_) => Integer
    case ArrayType(_) => Array
    case FunctionType(_, _) => Function
    case DoubleType(_) => Double
    case CharType(_) => Char
    case BooleanType(_) => Boolean
    case ObjectType(_) => Object
    case UnitType => Unit
  }

  sealed trait Enum
  case object Integer extends Enum
  case object Array extends Enum
  case object Function extends Enum
  case object Double extends Enum
  case object Char extends Enum
  case object Boolean extends Enum
  case object Object extends Enum
  case object Unit extends Enum
}

sealed trait TopType

case class IntegerType(value: Int) extends TopType {
  override def toString: String = s"$value"
}

case class ArrayType[T <: TopType](value: mutable.ArrayBuffer[T]) extends TopType {
  override def toString: String = Builtin.show(this)
}

case class FunctionType(value: FunctionLiteral,
                        closures: List[(String, ObjectInMemory)] = List.empty)
    extends TopType {
  override def toString: String = "Function"
}

case class DoubleType(value: Double) extends TopType {
  override def toString: String = s"$value"
}

case class CharType(value: Char) extends TopType {
  override def toString: String = value.toString
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

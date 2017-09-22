package io.github.typeness.intp

sealed trait Type

case object IntegerType extends Type {
  override def toString: String = "Integer"
}
case object ArrayType extends Type {
  override def toString: String = "Array"
}
case object FunctionType extends Type {
  override def toString: String = "Function"
}
case object DoubleType extends Type {
  override def toString: String = "Double"
}
case object CharType extends Type {
  override def toString: String = "Char"
}
case object BooleanType extends Type {
  override def toString: String = "Boolean"
}
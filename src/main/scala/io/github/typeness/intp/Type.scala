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

case object ObjectType extends Type {
  override def toString: String = "Object"
}

object Type {
  def getType(expr: Any): Type = expr match {
    case _: Int       => IntegerType
    case _: Seq[_]    => ArrayType
    case _: Double    => DoubleType
    case _: Char      => CharType
    case _: Boolean   => BooleanType
    case _: Map[_, _] => ObjectType
    case _: FunctionLiteral => FunctionType
    case _            => throw new Exception(s"Internal error: unknown type of $expr")
  }
}

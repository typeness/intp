package io.github.typeness.intp

abstract class Error(msg: String) extends Exception("error: " + msg)

abstract class InterpreterError(msg: String) extends Error(msg)

case class SyntaxError(msg: String) extends Error("invalid syntax " + msg)

case class TypeMismatch(found: Any, required: Type) extends InterpreterError(
  "type mismatch\n" +
    s"found: $found\n" +
    s"required: $required\n"
)

case class WrongUnaryOperator(op: Token, value: Any) extends InterpreterError(
  s"bad operand type for unary operator: ${op.value} $value"
)

case class WrongBinaryOperator(left: Any, op: Token, right: Any) extends InterpreterError(
  s"bad operands type for binary operator: $left ${op.value} $right"
)

case class UndefinedVariable(name: String) extends InterpreterError(
  s"'$name' is not defined"
)

case class WrongFunctionCall(name: String, provided: Int, excepted: Int) extends InterpreterError(
  s"function '$name' takes $excepted arguments ($provided given)"
)
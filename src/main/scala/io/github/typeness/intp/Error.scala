package io.github.typeness.intp

case class CompilationUnit(fileName: String, source: String)

abstract class Error(msg: String, compilationUnit: CompilationUnit, position: Position)
    extends Exception(
      s"${if (compilationUnit.fileName.nonEmpty) compilationUnit.fileName
      else "<console>"}:\n" + Console.RED +
        s"line:${position.row}: error: " + msg + Console.RESET +
        compilationUnit.source.split("\n").toVector(position.row - 1) + "\n" +
        (" " * position.col) + "^"
    )

abstract class InterpreterError(msg: String, compilationUnit: CompilationUnit, position: Position)
    extends Error(msg, compilationUnit, position)

case class SyntaxError(msg: String, compilationUnit: CompilationUnit, position: Position)
    extends Error(
      "invalid " +
        "syntax " + msg + "\n",
      compilationUnit,
      position
    )

case class AssertionError(compilationUnit: CompilationUnit, position: Position)
    extends InterpreterError(
      "assertion failed\n",
      compilationUnit,
      position
    )

case class TypeMismatch(found: TopType,
                        required: Type.Enum,
                        compilationUnit: CompilationUnit,
                        position: Position)
    extends InterpreterError(
      "type mismatch\n" +
        s"found: '${Type.toEnum(found).toString}' with value '$found'\n" +
        s"required: '$required'\n",
      compilationUnit,
      position
    )

case class CastError(from: TopType, to: Type.Enum, compilationUnit: CompilationUnit, position: Position)
    extends InterpreterError(
      "casting failed\n" +
        s"from: $from\n" +
        s"to: $to\n",
      compilationUnit,
      position
    )

case class WrongUnaryOperator(op: Token,
                              operand: Type.Enum,
                              compilationUnit: CompilationUnit,
                              position: Position)
    extends InterpreterError(
      s"bad operand's type '${operand.toString}' for unary operator' ${op.value}' \n",
      compilationUnit,
      position
    )

case class WrongBinaryOperator(left: Type.Enum,
                               op: Token,
                               right: Type.Enum,
                               compilationUnit: CompilationUnit,
                               position: Position)
    extends InterpreterError(
      s"bad operand's type for binary operator: '${left.toString}' '${op.value}' '${right.toString}'\n",
      compilationUnit,
      position
    )

case class UndefinedVariable(name: String, compilationUnit: CompilationUnit, position: Position)
    extends InterpreterError(
      s"'$name' is not defined\n",
      compilationUnit,
      position
    )

case class ValAssignment(name: String, compilationUnit: CompilationUnit, position: Position)
    extends InterpreterError(
      s"assignment to immutable variable '$name'\n",
      compilationUnit,
      position
    )

case class ObjectRedefinition(name: String, compilationUnit: CompilationUnit, position: Position)
    extends InterpreterError(
      s"'$name' is already defined in this scope\n",
      compilationUnit,
      position
    )

case class WrongFunctionCall(name: String,
                             provided: Int,
                             excepted: Int,
                             compilationUnit: CompilationUnit,
                             position: Position)
    extends InterpreterError(
      s"function '$name' takes $excepted arguments ($provided given)\n",
      compilationUnit,
      position
    )

case class IndexOutOfBound(name: String,
                           size: Int,
                           index: Int,
                           cu: CompilationUnit,
                           position: Position)
    extends InterpreterError(
      s"array '$name' has $size elements (element number $index accessed)\n",
      cu,
      position
    )

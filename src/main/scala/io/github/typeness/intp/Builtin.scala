package io.github.typeness.intp


import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Builtin {

  private def show(arg: Any): String = arg match {
    case seq: Seq[_] =>
      "[" + seq.map(show).mkString(", ") + "]"
    case map: mutable.Map[_, _] =>
      "{" + map.map { case (k, v) => s"$k = ${show(v)}" }.mkString(", ") + "}"
    case FunctionLiteral(_, _, token) =>
      token.value
    case value =>
      value.toString
  }

  type BuiltinFunction = ((Any, CompilationUnit, Position) => Any)

  private val string: BuiltinFunction  =
    (arg: Any, _: CompilationUnit, _: Position) => show(arg).to[mutable.ArrayBuffer]

  private val println: BuiltinFunction  =
    (arg: Any, _: CompilationUnit, _: Position) => scala.Console.println(show(arg))

  private val print: BuiltinFunction =
    (arg: Any, _: CompilationUnit, _: Position) => scala.Console.print(show(arg))

  private val size: BuiltinFunction =
    (arg: Any, compilationUnit: CompilationUnit, position: Position) =>
      arg match {
        case seq: Seq[_] => seq.size
        case value => throw TypeMismatch(value, ArrayType, compilationUnit, position)
      }

  private val read: BuiltinFunction =
    (arg: Any, compilationUnit: CompilationUnit, position: Position) =>
      ZeroArgumentFunction(
        arg,
        compilationUnit,
        position,
        "read",
        () => scala.io.StdIn.readLine().to[mutable.ArrayBuffer]
      )

  private val assert: BuiltinFunction =
    (arg: Any,
     compilationUnit: CompilationUnit,
     position: Position) =>
      arg match {
        case bool: Boolean => if(!bool) throw AssertionError(compilationUnit, position)
        case value         => throw TypeMismatch(value, BooleanType, compilationUnit, position)
      }

  private val int: BuiltinFunction =
    (arg: Any,
     compilationUnit: CompilationUnit,
     position: Position) =>
      arg match {
        case int: Int       => int
        case char: Char     => char.toInt
        case double: Double => double.toInt
        case seq: Seq[_] => Try(seq.mkString.toInt) match {
          case Success(x) => x
          case Failure(_) =>  throw TypeMismatch(seq.mkString, IntegerType, compilationUnit, position)
        }
        case value =>
          throw CastError(Type.getType(value), IntegerType, compilationUnit, position)
      }

  private val char: BuiltinFunction =
    (arg: Any,
     compilationUnit: CompilationUnit,
     position: Position) =>
      arg match {
        case int: Int       => int.toChar
        case char: Char     => char
        case double: Double => double.toChar
        case seq: Seq[_] => Try(seq.head.toString.charAt(0)) match {
          case Success(x) => x
          case Failure(_) =>  throw TypeMismatch(seq.mkString, CharType, compilationUnit, position)
        }
        case value =>
          throw CastError(Type.getType(value), CharType, compilationUnit, position)
      }

  private val double: BuiltinFunction =
    (arg: Any,
     compilationUnit: CompilationUnit,
     position: Position) =>
      arg match {
        case int: Int       => int.toDouble
        case char: Char     => char.toDouble
        case double: Double => double
        case seq: Seq[_] => Try(seq.mkString.toDouble) match {
          case Success(x) => x
          case Failure(_) =>  throw TypeMismatch(seq.mkString, DoubleType, compilationUnit, position)
        }
        case value =>
          throw CastError(Type.getType(value), DoubleType, compilationUnit, position)
      }

  val functions: Map[String, BuiltinFunction] = Map(
    "println" -> println,
    "print" -> print,
    "size" -> size,
    "string" -> string,
    "read" -> read,
    "assert" -> assert,
    "int" -> int,
    "char" -> char,
    "double" -> double
  )

  private object ZeroArgumentFunction {
    def apply(arg: Any,
              compilationUnit: CompilationUnit,
              position: Position,
              name: String,
              fn: () => Any): Any = arg match {
      case seq: Seq[_] if seq.isEmpty => fn()
      case s: Seq[_]                  => throw WrongFunctionCall(name, s.size, 0, compilationUnit, position)
      case _                          => throw WrongFunctionCall(name, 1, 0, compilationUnit, position)
    }
  }

}

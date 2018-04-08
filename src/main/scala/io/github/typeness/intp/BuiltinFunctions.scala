package io.github.typeness.intp


import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BuiltinFunctions {

  private val string =
    (arg: Any, cu: CompilationUnit, pos: Position) => show(arg).to[mutable.ArrayBuffer]

  private def show: Any => String =  (arg: Any) => {
    val result = arg match {
      case seq: Seq[_] => "[" + seq.map(show).mkString(", ") + "]"
      case map: mutable.Map[_, _] =>
          "{" + map.map { case (k, v) => s"$k = ${show(v)}" }.mkString(", ") + "}"
      case FunctionLiteral(_, _, token) => token.value
      case value => value.toString
    }
    result
//    result.to[mutable.ArrayBuffer]
  }

  val map: Map[String, (Any, CompilationUnit, Position) => Any] = Map(
    "println" -> ((arg: Any,
                   compilationUnit: CompilationUnit,
                   position: Position) => println(show(arg))),
    "print" -> ((arg: Any,
                   compilationUnit: CompilationUnit,
                   position: Position) => print(show(arg))),
    "size" -> ((arg: Any,
                compilationUnit: CompilationUnit,
                position: Position) =>
                 arg match {
                   case seq: Seq[_] => seq.size
                   case value       => throw TypeMismatch(value, ArrayType, compilationUnit, position)
                 }),
    "string" -> string,
    "read" -> ((arg: Any,
                    compilationUnit: CompilationUnit,
                    position: Position) =>
                     ZeroArgumentFunction(
                       arg,
                       compilationUnit,
                       position,
                       "read",
                       () => scala.io.StdIn.readLine().to[mutable.ArrayBuffer]
                     )),
    "assert" -> ((arg: Any,
                  compilationUnit: CompilationUnit,
                  position: Position) =>
                   arg match {
                     case bool: Boolean => if(!bool) throw AssertionError(compilationUnit, position)
                     case value         => throw TypeMismatch(value, BooleanType, compilationUnit, position)
                   }),
    "int" -> ((arg: Any,
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
                }),
    "char" -> ((arg: Any,
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
                 }),
    "double" -> ((arg: Any,
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
                   })
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

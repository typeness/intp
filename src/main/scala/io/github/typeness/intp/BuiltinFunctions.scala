package io.github.typeness.intp

import scala.collection.mutable

object BuiltinFunctions {
  val map: Map[String, (Any, CompilationUnit, Position) => Any] = Map(
    "println" -> ((arg: Any,
                   compilationUnit: CompilationUnit,
                   position: Position) =>
                    arg match {
                      case seq: Seq[_] => println("[" + seq.mkString(", ") + "]")
                      //                      case seq: Seq[Char] => println(seq)
                      case map: Map[_, _] =>
                        println(
                          "{" + map.map { case (k, v) => s"$k = $v" }.mkString(", ") + "}"
                        )
                      case x => println(x)
                    }),
    "size" -> ((arg: Any,
                compilationUnit: CompilationUnit,
                position: Position) =>
                 arg match {
                   case seq: Seq[_] => seq.size
                   case value       => throw TypeMismatch(value, ArrayType, compilationUnit, position)
                 }),
    "print" -> ((arg: Any,
                 compilationUnit: CompilationUnit,
                 position: Position) =>
                  arg match {
                    case seq: Seq[_] => print("[" + seq.mkString(", ") + "]")
                    case map: Map[_, _] =>
                      print(
                        "{" + map.map { case (k, v) => s"$k = $v" }.mkString(", ") + "}"
                      )
                    case x => print(x)
                  }),
    "string" -> ((arg: Any, compilationUnit: CompilationUnit, position: Position) => {
      arg match {
        case value => value.toString.to[mutable.ArrayBuffer]
      }
    }),
    "readInt" -> ((arg: Any,
                   compilationUnit: CompilationUnit,
                   position: Position) =>
                    ZeroArgumentFunction(
                      arg,
                      compilationUnit,
                      position,
                      "readInt",
                      () => scala.io.StdIn.readInt()
                    )),
    "readDouble" -> ((arg: Any,
                      compilationUnit: CompilationUnit,
                      position: Position) =>
                       ZeroArgumentFunction(
                         arg,
                         compilationUnit,
                         position,
                         "readDouble",
                         () => scala.io.StdIn.readDouble()
                       )),
    "readLine" -> ((arg: Any,
                    compilationUnit: CompilationUnit,
                    position: Position) =>
                     ZeroArgumentFunction(
                       arg,
                       compilationUnit,
                       position,
                       "readLine",
                       () => scala.io.StdIn.readLine()
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

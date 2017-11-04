package io.github.typeness.intp

object BuiltinFunctions {
  val map: Map[String, (Any, CompilationUnit, Position) => Any] = Map(
    "println" -> ((arg: Any,
                   compilationUnit: CompilationUnit,
                   position: Position) =>
                    arg match {
                      case seq: Seq[_] => println("[" + seq.mkString(", ") + "]")
                      //                      case seq: Seq[Char] => println(seq)
                      case x => println(x)
                    }),
    "size" -> ((arg: Any, compilationUnit: CompilationUnit, position: Position) => {
      arg match {
        case seq: Seq[_] => seq.size
        case value       => throw TypeMismatch(value, ArrayType, compilationUnit, position)
      }
    }),
    "print" -> ((arg: Any,
                 compilationUnit: CompilationUnit,
                 position: Position) =>
                  arg match {
                    case seq: Seq[_] => print("[" + seq.mkString(", ") + "]")
                    case x           => println(x)
                  }),
    "string" -> ((arg: Any, compilationUnit: CompilationUnit, position: Position) => {
      arg match {
        case seq: Seq[_] => "[" + seq.mkString(", ") + "]"
        case value       => value.toString
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
                     ))
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

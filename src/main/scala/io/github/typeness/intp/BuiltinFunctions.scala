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
    })
  )
}

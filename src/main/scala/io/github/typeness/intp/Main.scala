package io.github.typeness.intp

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println("No input file given")
    else {
      try {
        val intp = new Interpreter()
        intp.runFromFile(args(0))
      } catch {
        case e: Throwable => println(e.getMessage)
      }
    }
  }
}

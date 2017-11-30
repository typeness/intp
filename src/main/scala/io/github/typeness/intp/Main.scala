package io.github.typeness.intp

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println("No input file given")
    else {
      val source = scala.io.Source.fromFile(args(0)).mkString
      val intp = new Interpreter()
      try {
        intp.runFromString(source)
      } catch {
        case e: Exception => println(e.getMessage)
      }
    }
  }
}

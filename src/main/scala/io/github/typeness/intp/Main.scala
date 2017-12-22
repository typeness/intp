package io.github.typeness.intp

object Main {
  private val version: String = "0.3.1"

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println("No input file given")
    else
      args(0) match {
        case "-v" => println(version)
        case _ =>
          try {
            val intp = new Interpreter()
            intp.runFromFile(args(0))
          } catch {
            case e: Throwable => println(e.getMessage)
          }
      }
  }
}

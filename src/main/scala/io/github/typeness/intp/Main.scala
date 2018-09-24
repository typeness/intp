package io.github.typeness.intp

object Main {
  private val version: String = "0.5.1"

  def main(args: Array[String]): Unit = {
    val debug = args.length > 1 && args(1) == "-d"
    if (args.isEmpty) println("No input file given")
    else
      args(0) match {
        case "-v" => println(version)
        case filename =>
          try {
            val intp = new Interpreter()
            val _ = intp.runFromFile(filename)
          } catch {
            case e: Throwable =>
              if (debug) throw e
              else println(e.getMessage)
          }
      }
  }
}

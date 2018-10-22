package io.github.typeness.intp

import scala.io.StdIn

class Repl {
  var buffer: List[String] = Nil
  val interpreter = new Interpreter()

  private def enter(line: String): Unit = line match {
    case "GO" =>
      buffer = buffer.reverse
      val _ = interpreter.runFromString(buffer.mkString("\n"))
      buffer = Nil
    case _ =>
      buffer = line :: buffer
  }

  def run(debug: Boolean): Unit = {
    try {
      val line = StdIn.readLine("intp> ")
      enter(line)
      run(debug)
    } catch {
      case e: Error =>
        if (debug) throw e
        else println(e.getMessage)
        buffer = Nil
        run(debug)
    }
  }
}

package io.github.typeness.intp

object Main {
  private val version: String = "0.5.1"

  def main(args: Array[String]): Unit = {
    val argsList = args.toList
    val debug = argsList.contains("-d")
      args.toList match {
        case "-v" :: Nil =>
          println(version)
        case "-d" :: Nil | Nil =>
            val repl = new Repl()
            repl.run(debug)
        case filename :: _ =>
          try {
            val intp = new Interpreter()
            val _ = intp.runFromFile(filename)
          } catch {
            case e: Error =>
              if (debug) throw e
              else println(e.getMessage)
          }

      }
  }
}

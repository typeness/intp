package io.github.typeness.intp

import org.scalatest.FunSuite

class RuntimeTest extends FunSuite {
  test("Selection sort") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/selectionSort.intp")
  }
}

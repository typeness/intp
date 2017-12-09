package io.github.typeness.intp

import org.scalatest.FunSuite

class RuntimeTest extends FunSuite {
  test("Bubble sort") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/bubbleSort.intp")
  }
  test("Binary search") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/binSearch.intp")
  }
}
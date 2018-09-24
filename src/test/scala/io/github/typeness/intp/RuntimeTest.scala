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
  test("Union, intersection and difference on sets") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/sets.intp")
  }
  test("Count occurrences in string") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/countOccurrences.intp")
  }
  test("Binary to decimal conversion") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/binToInt.intp")
  }
  test("Checking for palindrome") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/palindrome.intp")
  }
  test("Insertion sort") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/insertionSort.intp")
  }
  test("Merge sort") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/mergeSort.intp")
  }
  test("Quick sort") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/quickSort.intp")
  }
  test("Perfect square") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("runtime/isPerfectSquare.intp")
  }
}

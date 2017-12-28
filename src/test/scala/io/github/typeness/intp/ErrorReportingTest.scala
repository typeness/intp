package io.github.typeness.intp

import org.scalatest.FunSuite

class ErrorReportingTest extends FunSuite {
  test("Type mismatch: indexing non-array object") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-object.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: non-boolean in while loop condition") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/loop-condition.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: non-boolean in if condition") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/if-condition.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: calling non-function object") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/function-call.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: indexing array with non-integer object") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-index.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: assign to not array like it's array") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-assign.intp")
    }
    println(error.getMessage)
  }
  test("Type mismatch: assign array by wrong index") {
    val error = intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-assign-index.intp")
    }
    println(error.getMessage)
  }
  test("Invalid syntax: use invalid operator &") {
    val error = intercept[SyntaxError] {
      val parser = new Parser("&1")()
      parser.parse()
    }
    println(error.getMessage)
  }
  test("Wrong unary operator: type mismatch") {
    val error = intercept[WrongUnaryOperator] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/unary-op-mismatch.intp")
    }
    println(error.getMessage)
  }
  test("Wrong binary operator: type mismatch") {
    val error = intercept[WrongBinaryOperator] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/binary-op-mismatch.intp")
    }
    println(error.getMessage)
  }
  test("Use undefined variable") {
    val error = intercept[UndefinedVariable] {
      val interpreter = new Interpreter()
      interpreter.runFromString("a = b")
    }
    println(error.getMessage)
  }
  test("Provide wrong number of arguments for function call") {
    val error = intercept[WrongFunctionCall] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/wrong-function-call.intp")
    }
    println(error.getMessage)
  }
  test("Provide wrong number of for anonymous function call") {
    val error = intercept[WrongFunctionCall] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/wrong-anon-function-call.intp")
    }
    println(error.getMessage)
  }
  test("Failed assertion") {
    val error = intercept[AssertionError] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/failed-assertion.intp")
    }
    println(error.getMessage)
  }
}

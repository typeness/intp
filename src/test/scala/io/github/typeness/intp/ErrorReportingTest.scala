package io.github.typeness.intp

import org.scalatest.FunSuite

class ErrorReportingTest extends FunSuite {
  test("Type mismatch: indexing non-array object") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-object.intp")
    }
  }
  test("Type mismatch: non-boolean in while loop condition") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/loop-condition.intp")
    }
  }
  test("Type mismatch: non-boolean in if condition") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/if-condition.intp")
    }
  }
  test("Type mismatch: calling non-function object") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/function-call.intp")
    }
  }
  test("Type mismatch: indexing array with non-integer object") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-index.intp")
    }
  }
  test("Type mismatch: assign to not array like it's array") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-assign.intp")
    }
  }
  test("Type mismatch: assign array by wrong index") {
    intercept[TypeMismatch] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/array-assign-index.intp")
    }
  }
  test("Invalid syntax: use invalid operator &") {
    intercept[SyntaxError] {
      val parser = new Parser("&1")
      parser.parse()
    }
  }
  test("Wrong unary operator: type mismatch") {
    intercept[WrongUnaryOperator] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/unary-op-mismatch.intp")
    }
  }
  test("Wrong binary operator: type mismatch") {
    intercept[WrongBinaryOperator] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/binary-op-mismatch.intp")
    }
  }
  test("Use undefined variable") {
    intercept[UndefinedVariable] {
      val interpreter = new Interpreter()
      interpreter.runFromString("a = b")
    }
  }
  test("Provide wrong number of arguments for function call") {
    intercept[WrongFunctionCall] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/wrong-function-call.intp")
    }
  }
  test("Provide wrong number of for anonymous function call") {
    intercept[WrongFunctionCall] {
      val interpreter = new Interpreter()
      interpreter.runFromResource("errors/wrong-anon-function-call.intp")
    }
  }
}

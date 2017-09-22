package io.github.typeness.intp

import org.scalatest.FunSuite

class ErrorReportingTest extends FunSuite {
  test("Type mismatch: indexing non-array object") {
    val parser = Parser.fromResource("errors/array-object.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: non-boolean in while loop condition") {
    val parser = Parser.fromResource("errors/loop-condition.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: non-boolean in if condition") {
    val parser = Parser.fromResource("errors/if-condition.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: calling non-function object") {
    val parser = Parser.fromResource("errors/function-call.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: indexing array with non-integer object") {
    val parser = Parser.fromResource("errors/array-index.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: assign to not array like it's array") {
    val parser = Parser.fromResource("errors/array-assign.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Type mismatch: assign array by wrong index") {
    val parser = Parser.fromResource("errors/array-assign-index.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[TypeMismatch] {
      interpreter.visit(ast)
    }
  }
  test("Invalid syntax: use invalid operator &") {
    intercept[SyntaxError] {
      val parser = new Parser("&1")
      parser.parse()
    }
  }
  test("Wrong unary operator: type mismatch") {
    val parser = Parser.fromResource("errors/unary-op-mismatch.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[WrongUnaryOperator] {
      interpreter.visit(ast)
    }
  }
  test("Wrong binary operator: type mismatch") {
    val parser = Parser.fromResource("errors/binary-op-mismatch.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[WrongBinaryOperator] {
      interpreter.visit(ast)
    }
  }
  test("Use undefined variable") {
    val parser = new Parser("a = b")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[UndefinedVariable] {
      interpreter.visit(ast)
    }
  }
  test("Provide wrong number of arguments for function call") {
    val parser = Parser.fromResource("errors/wrong-function-call.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[WrongFunctionCall] {
      interpreter.visit(ast)
    }
  }
  test("Provide wrong number of for anonymous function call") {
    val parser = Parser.fromResource("errors/wrong-anon-function-call.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    intercept[WrongFunctionCall] {
      interpreter.visit(ast)
    }
  }
}

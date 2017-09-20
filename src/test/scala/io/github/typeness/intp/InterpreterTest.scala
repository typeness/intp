package io.github.typeness.intp

import org.scalatest.FunSuite

class InterpreterTest extends FunSuite {
  test("Calculate arithmetic expression") {
    val parser = new Parser("2+3")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 2 + 3)
  }
  test("Allow multiple digits in input") {
    val parser = new Parser("12+14")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 12 + 14)
  }
  test("Handle input with whitespace characters") {
    val parser = new Parser("10 + 1")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 10 + 1)
  }
  test("Evaluate subtractions") {
    val parser = new Parser("7 - 5")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 7 - 5)
  }
  test("Handle multiplication") {
    val parser = new Parser("3*8")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 3 * 8)
  }
  test("Handle division") {
    val parser = new Parser("8/4")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 8 / 4)
  }
  test("Evaluate arbitrary number of additions and subtractions") {
    val parser = new Parser("9 - 5 + 3 + 11")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 9 - 5 + 3 + 11)
  }
  test("Evaluate single number") {
    val parser = new Parser("12345")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 12345)
  }
  test("Evaluate arbitrary expression") {
    val parser = new Parser("10-2*3/4+5-1")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 10 - 2 * 3 / 4 + 5 - 1)
  }
  test("Handle order of subtraction") {
    val parser = new Parser("7 - 3 - 1")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 7 - 3 - 1)
  }
  test("Handle parenthesis") {
    val parser = new Parser("7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + 8)
  }
  test("Handle unary minus") {
    val parser = new Parser("--2")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == -(-2))
  }
  test("Evaluate arbitrary number of unary operators") {
    val parser = new Parser("5 - - - + - (3 + 4) - +2")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == 10)
  }
  test("Numeric variable assign and read") {
    val parser = Parser.fromResource("interpreter/variables.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("x" -> 2, "y" -> 8, "z" -> 10)
    )
  }
  test("Boolean expression") {
    val parser = new Parser("true or false and not true")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == true)
  }
  test("If statement") {
    val parser = Parser.fromResource("interpreter/if.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("is" -> true, "d" -> 1000)
    )
  }
  test("If-else statements") {
    val parser = Parser.fromResource("interpreter/if-else.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("x" -> 2, "c" -> true)
    )
  }
  test("While statement") {
    val parser = Parser.fromResource("interpreter/while.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("x" -> 1000000)
    )
  }
  test("Assignment of array literal") {
    val parser = new Parser("x = [1, 2, 3]")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    val x = interpreter.globalScope.get("x")
    assert(
      interpreter.globalScope == Map("x" -> Vector(1, 2, 3))
    )
  }
  test("Assignment of multi-dimension array literal") {
    val parser = new Parser("x = [[1, 2], [3, 4], [5]]")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    val x = interpreter.globalScope.get("x")
    assert(
      interpreter.globalScope == Map("x" -> Vector(Vector(1, 2), Vector(3, 4), Vector(5)))
    )
  }
  test("Concat of array literal") {
    val parser = new Parser("x = [1, 2, 3] + [4, 5, 6]")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("x" -> (Vector(1, 2, 3) ++ Vector(4, 5, 6)))
    )
  }
  test("Push 10 elements to array in loop") {
    val parser = Parser.fromResource("interpreter/array-loop.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("arr" -> Vector.range(0, 10), "x" -> 10)
    )
  }
  test("Array access") {
    val parser = Parser.fromResource("interpreter/array-access.intp")
    val ast = parser.parse()
    val interpreter = new Interpreter()
    interpreter.visit(ast)
    assert(
      interpreter.globalScope == Map("arr" -> Vector(-100, 33, 55, 56, 234), "middle" -> 55)
    )
  }
}
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
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/variables.intp")
    assert(
      interpreter.memory.getAll == Map("x" -> 2, "y" -> 8, "z" -> 10)
    )
  }
  test("Boolean expression") {
    val parser = new Parser("true or false and not true")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == true)
  }
  test("If statement") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/if.intp")
    assert(
      interpreter.memory.getAll == Map("is" -> true, "d" -> 1000)
    )
  }
  test("If-else statements") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/if-else.intp")
    assert(
      interpreter.memory.getAll == Map("x" -> 2, "c" -> true)
    )
  }
  test("While statement") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/while.intp")
    assert(
      interpreter.memory.getAll == Map("x" -> 1000000)
    )
  }
  test("Assignment of array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("x = [1, 2, 3]")
    assert(
      interpreter.memory.getAll == Map("x" -> Vector(1, 2, 3))
    )
  }
  test("Assignment of multi-dimension array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("x = [[1, 2], [3, 4], [5]]")
    assert(
      interpreter.memory.getAll == Map("x" -> Vector(Vector(1, 2), Vector(3, 4), Vector(5)))
    )
  }
  test("Concat of array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("x = [1, 2, 3] + [4, 5, 6]")
    assert(
      interpreter.memory.getAll == Map("x" -> (Vector(1, 2, 3) ++ Vector(4, 5, 6)))
    )
  }
  test("Push 10 elements to array in loop") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/array-loop.intp")
    assert(
      interpreter.memory.getAll == Map("arr" -> Vector.range(0, 10), "x" -> 10)
    )
  }
  test("Array access") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/array-access.intp")
    assert(
      interpreter.memory.getAll == Map("arr" -> Vector(-100, 33, 55, 56, 234), "middle" -> 55)
    )
  }
  test("Character assignment") {
    val interpreter = new Interpreter()
    interpreter.runFromString("x = 'c'")
    assert(
      interpreter.memory.getAll == Map("x" -> 'c')
    )
  }
  test("Define a function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/functionDefinition.intp")
    assert(
      interpreter.memory.getAll == Map(
        "f" -> FunctionLiteral(List(IdToken("a"), IdToken("b")),
          Program(List(AssignAST(IdToken("x"),
            BinOp(VarAST(IdToken("a")), MultiplicationToken, Number(IntegerConstToken(2)))),
            AssignAST(IdToken("var"),
              ArrayLiteral(List(VarAST(IdToken("a")), VarAST(IdToken("b")), VarAST(IdToken("x")))))))))
    )
  }
  test("Call a function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/function-call.intp")
    interpreter.memory.getAll == Map(
      "arr" -> Vector(44, 88, 132, 176),
      "x" -> 44,
      "f" -> FunctionLiteral(
        List(IdToken("a")),
        Program(
          List(
            ReturnAST(
              ArrayLiteral(
                List(
                  VarAST(IdToken("a")),
                  BinOp(
                    Number(IntegerConstToken(2)),
                    MultiplicationToken,
                    VarAST(IdToken("a"))
                  ),
                  BinOp(
                    Number(IntegerConstToken(3)),
                    MultiplicationToken, VarAST(IdToken("a"))
                  ),
                  BinOp(
                    Number(IntegerConstToken(4)),
                    MultiplicationToken,
                    VarAST(IdToken("a"))
                  )
                )
              )
            )
          )
        )
      )
    )
  }
  test("Return function from function and call it") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/return-function.intp")
    assert(
      interpreter.memory.getAll == Map(
        "x" -> 3,
        "f" -> FunctionLiteral(
          List(),
          Program(List(ReturnAST(FunctionLiteral(List(), Program(List(ReturnAST(Number(IntegerConstToken(3)))))))))
        )
      )
    )
  }
  test("Call function with another function as argument") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/function-as-argument.intp")
    assert(
      interpreter.memory.getAll == Map(
        "result" -> true,
        "g" -> FunctionLiteral(
          List(),
          Program(List(ReturnAST(BooleanLiteral(TrueToken))))),
        "f" -> FunctionLiteral(List(IdToken("x")),
          Program(List(ReturnAST(FunctionCall(VarAST(IdToken("x")), List())))))
      )
    )
  }
  test("Assignment for array indexed object") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/assignment-array-indexed.intp")
    assert(
      interpreter.memory.getAll == Map("x" -> Vector(1, 2, 10))

    )
  }
  test("Strings manipulation") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/strings-manipulation.intp")
    assert(
      interpreter.memory.getAll == Map(
        "z" -> Vector('A', 'l', 'a', ' ', 'm', 'a', ' ', 'k', 'o', 't', 'a', '.'),
        "y" -> Vector(' ', 'm', 'a'),
        "x" -> Vector('A', 'l', 'a'),
        "c" -> 'A'
      )
    )
  }
  test("Operator == for all possible types") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/equals.intp")
    assert(
      interpreter.memory.getAll == Map(
        "e" -> false, "j" -> false, "f" -> true, "a" -> true, "i" -> true, "b" -> false, "g" -> true, "c" -> true, "h" -> true, "d" -> false
      )
    )
  }
  test("Compute recursion factorial") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/factorial.intp")
    assert(interpreter.memory.get("result").contains(120))
  }
  test("Operator != for all possible types") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/not-equals.intp")
    assert(
      interpreter.memory.getAll == Map(
        "e" -> true, "j" -> true, "f" -> false, "a" -> false, "i" -> false, "b" -> true, "g" -> false, "c" -> false, "h" -> false, "d" -> true
      )
    )
  }
  test("Run empty program") {
    val interpreter = new Interpreter()
    interpreter.runFromString("")
    assert(
      interpreter.memory.getAll == Map.empty
    )
  }
  test("Calculate with modulo operator") {
    val parser = new Parser("10 % 3")
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(
      interpreter.visit(ast) == 10 % 3
    )
  }
  test("Allow reuse of functions literal") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/f-literal-reuse.intp")
    assert(
      interpreter.memory.getAll == Map(
        "f1" -> FunctionLiteral(List(IdToken("a")), Program(List(ReturnAST(VarAST(IdToken("a")))))),
        "f2" -> FunctionLiteral(List(IdToken("a")), Program(List(ReturnAST(VarAST(IdToken("a")))))),
        "test" -> true
      )
    )
  }
}

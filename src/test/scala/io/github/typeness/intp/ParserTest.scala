package io.github.typeness.intp

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("Parse single number") {
    val parser = new Parser("1")
    val ast = parser.parse().children.head
    assert(ast == Number(IntegerConstToken(1)))
  }
  test("Parse assignment") {
    val parser = new Parser("x = (2.0 - 1) * 2")
    val ast = parser.parse().children.head
    assert(ast == AssignAST(IdToken("x"),
      BinOp(BinOp(Number(RealConstToken(2.0)), SubtractionToken, Number(IntegerConstToken(1))),
        MultiplicationToken, Number(IntegerConstToken(2)))))
  }
  test("Parse multiplication") {
    val parser = new Parser("2 * 3")
    val ast = parser.parse().children.head
    assert(ast == BinOp(Number(IntegerConstToken(2)), MultiplicationToken, Number(IntegerConstToken(3))))
  }
  test("Parse addition") {
    val parser = new Parser("13 + 10")
    val ast = parser.parse().children.head
    assert(ast == BinOp(Number(IntegerConstToken(13)), AdditionToken, Number(IntegerConstToken(10))))
  }
  test("Parse division and subtraction") {
    val parser = new Parser("50 - 10 / 2")
    val ast = parser.parse().children.head
    assert(ast == BinOp(Number(IntegerConstToken(50)), SubtractionToken,
      BinOp(Number(IntegerConstToken(10)), DivisionToken, Number(IntegerConstToken(2)))))
  }
  test("Parse arbitrary number of unary operators") {
    val parser = new Parser("--+999")
    val ast = parser.parse().children.head
    assert(ast == UnaryOp(SubtractionToken, UnaryOp(SubtractionToken,
      UnaryOp(AdditionToken, Number(IntegerConstToken(999))))))
  }
  test("Parse boolean expression") {
    val parser = new Parser("true or false and not true")
    val ast = parser.parse().children.head
    assert(ast ==
      BinOp(BooleanLiteral(TrueToken), OrToken,
        BinOp(BooleanLiteral(FalseToken), AndToken, UnaryOp(NotToken, BooleanLiteral(TrueToken)))
      )
    )
  }
  test("Parser arithmetic boolean expression") {
    val parser = new Parser("1 + 9 > 2 / 4.2")
    val ast = parser.parse().children.head
    assert(ast == BinOp(
      BinOp(Number(IntegerConstToken(1)), AdditionToken, Number(IntegerConstToken(9))),
      GreaterToken,
      BinOp(Number(IntegerConstToken(2)), DivisionToken, Number(RealConstToken(4.2)))
    )
    )
  }
  test("Parser complex arithmetic boolean expression") {
    val parser = new Parser("x + 1 > 3 or y * 3 == 4")
    val ast = parser.parse().children.head
    assert(ast ==
      BinOp(
        BinOp(
          BinOp(
            VarAST(IdToken("x")),
            AdditionToken,
            Number(IntegerConstToken(1))
          ),
          GreaterToken,
          Number(IntegerConstToken(3))
        ),
        OrToken,
        BinOp(
          BinOp(
            VarAST(IdToken("y")),
            MultiplicationToken,
            Number(IntegerConstToken(3))
          ),
          EqualsToken,
          Number(IntegerConstToken(4))
        )
      )
    )
  }
  test("Parse multiple statements") {
    val parser = Parser.fromResource("parser/multipleStatements.intp")
    val ast = parser.parse().children
    val expected = List(
      AssignAST(IdToken("x"), BinOp(Number(IntegerConstToken(1)), MultiplicationToken,
        Number(IntegerConstToken(2)))),
      AssignAST(IdToken("y"),
        BinOp(Number(IntegerConstToken(2)), AdditionToken, VarAST(IdToken("x")))),
      AssignAST(IdToken("abcd"), ArrayLiteral(List(
        BinOp(Number(IntegerConstToken(2)), MultiplicationToken, Number(IntegerConstToken(3))),
        Number(RealConstToken(1.3)), BinOp(VarAST(IdToken("y")), MultiplicationToken,
          VarAST(IdToken("x"))),
        BinOp(BooleanLiteral(TrueToken), OrToken, BooleanLiteral(FalseToken))))),
      BinOp(VarAST(IdToken("abcd")), MultiplicationToken, Number(IntegerConstToken(2))),
      FunctionCall(VarAST(IdToken("f")), List(VarAST(IdToken("abcd")),
        BinOp(ArrayAccess(VarAST(IdToken("g")), Number(IntegerConstToken(1))), MultiplicationToken,
          ArrayAccess(VarAST(IdToken("h")), Number(IntegerConstToken(2))))))
    )
    assert(ast == expected)
  }
  test("Parse functions call") {
    val parser = new Parser("f(3, false)\ng()")
    val ast = parser.parse().children
    val excepted = List(
      FunctionCall(VarAST(IdToken("f")), List(Number(IntegerConstToken(3)), BooleanLiteral(FalseToken))),
      FunctionCall(VarAST(IdToken("g")), List())
    )
    assert(ast == excepted)
  }
  test("Parser array literal") {
    val parser = new Parser("[1, 2, 3]")
    val ast = parser.parse().children
    val excepted = List(
      ArrayLiteral(List(Number(IntegerConstToken(1)), Number(IntegerConstToken(2)),
        Number(IntegerConstToken(3))))
    )
    assert(excepted == ast)
  }
  test("Parse array indexing") {
    val parser = new Parser("x[0]")
    val ast = parser.parse().children.head
    assert(ast ==
      ArrayAccess(VarAST(IdToken("x")), Number(IntegerConstToken(0)))
    )
  }
  test("Parse assignment for array indexed object") {
    val parser = new Parser("v[1] = 3")
    val ast = parser.parse().children.head
    assert(ast ==
      ArrayAssignAST(VarAST(IdToken("v")), Number(IntegerConstToken(1)), Number(IntegerConstToken(3)))
    )
  }
  test("Parse multiple dimension array literal") {
    val parser = new Parser("[[1, 2], [3, 4]]")
    val ast = parser.parse().children.head
    assert(ast ==
      ArrayLiteral(List(
        ArrayLiteral(List(
          Number(IntegerConstToken(1)),
          Number(IntegerConstToken(2))
        )),
        ArrayLiteral(List(
          Number(IntegerConstToken(3)),
          Number(IntegerConstToken(4))
        ))
      ))
    )
  }
  test("Parse multiple dimension array indexing") {
    val parser = new Parser("a[0][2]")
    val ast = parser.parse().children.head
    assert(ast ==
      ArrayAccess(ArrayAccess(VarAST(IdToken("a")), Number(IntegerConstToken(0))), Number(IntegerConstToken(2)))
    )
  }
  test("Parser chain function calls") {
    val parser = new Parser("f(1)(2)")
    val ast = parser.parse().children.head
    assert(ast ==
      FunctionCall(FunctionCall(VarAST(IdToken("f")), List(Number(IntegerConstToken(1)))), List(Number(IntegerConstToken(2))))
    )

  }
  test("Parse function definition") {
    val parser = Parser.fromResource("parser/functionDefinition.intp")
    val ast = parser.parse().children
    assert(ast == List(
      AssignAST(IdToken("f"), FunctionLiteral(List(IdToken("a"), IdToken("b")),
        Program(List(
          AssignAST(
            IdToken("x"), BinOp(VarAST(IdToken("a")), MultiplicationToken, Number(IntegerConstToken(2)))),
          AssignAST(
            IdToken("var"), ArrayLiteral(List(VarAST(IdToken("a")), VarAST(IdToken("b")), VarAST(IdToken("x"))))))))
      )
    ))
  }
  test("Parse character assignment") {
    val parser = new Parser("c = 'a'")
    val ast = parser.parse().children.head
    assert(ast ==
      AssignAST(IdToken("c"), CharLiteral(CharToken('a')))
    )
  }
  test("Parse string assignment") {
    val parser = new Parser("s = \"text\"")
    val ast = parser.parse().children.head
    assert(ast ==
      AssignAST(IdToken("s"), ArrayLiteral(List(
        CharLiteral(CharToken('t')), CharLiteral(CharToken('e')), CharLiteral(CharToken('x')), CharLiteral(CharToken('t'))
      )))
    )
  }
  test("Parse if statement") {
    val parser = Parser.fromResource("parser/if.intp")
    val ast = parser.parse().children.head
    assert(ast ==
      IfAST(BooleanLiteral(FalseToken),
        Program(List(FunctionCall(VarAST(IdToken("f")), List()))),
        None)
    )
  }
  test("Parse if-else statement") {
    val parser = Parser.fromResource("parser/if-else.intp")
    val ast = parser.parse().children.head
    assert(ast ==
      IfAST(BinOp(VarAST(IdToken("x")), EqualsToken, Number(IntegerConstToken(2))),
        Program(List(AssignAST(IdToken("c"), BooleanLiteral(TrueToken)))),
        Some(Program(List(AssignAST(IdToken("c"), BooleanLiteral(FalseToken)))))
      )
    )
  }
  test("Parse while statement") {
    val parser = Parser.fromResource("parser/while.intp")
    val ast = parser.parse().children.head
    assert(ast ==
      WhileAST(BooleanLiteral(TrueToken),
        Program(List(AssignAST(IdToken("x"), BinOp(VarAST(IdToken("x")), AdditionToken, Number(IntegerConstToken(1))))))
      )
    )
  }
  test("Parse function definition inside another function") {
    val parser = Parser.fromResource("parser/nestedFunctions.intp")
    val ast = parser.parse().children.head
    assert(ast ==
      AssignAST(IdToken("f"), FunctionLiteral(List(IdToken("a"), IdToken("b")),
        Program(List(AssignAST(IdToken("g"), FunctionLiteral(List(IdToken("c"), IdToken("d")),
          Program(List(FunctionCall(VarAST(IdToken("f")), List(VarAST(IdToken("c")), VarAST(IdToken("d"))))
          )))),
          FunctionCall(VarAST(IdToken("g")), List(VarAST(IdToken("a")), VarAST(IdToken("b"))))))
      ))
    )
  }
  test("Parse return statement") {
    val parser = Parser.fromResource("parser/return.intp")
    val ast = parser.parse().children.head
    assert(ast ==
      AssignAST(IdToken("f"),
        FunctionLiteral(
          List(IdToken("a")),
          Program(List(
            IfAST(BinOp(VarAST(IdToken("a")), GreaterToken, Number(IntegerConstToken(10))),
              Program(List(ReturnAST(BooleanLiteral(TrueToken)))), None),
            ReturnAST(BooleanLiteral(FalseToken))
          )))
      )
    )
  }
}

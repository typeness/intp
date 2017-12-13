package io.github.typeness.intp

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("Parse single number") {
    val parser = new Parser("1")()
    val ast = parser.parse().children.head
    assert(
      ast == Number(IntegerConstToken(1, Position(1, 1)))
    )

  }
  test("Parse assignment") {
    val parser = new Parser("x = (2.0 - 1) * 2")()
    val ast = parser.parse().children.head
    assert(
      ast == AssignAST(
        IdToken("x", Position(1, 1)),
        BinOp(
          BinOp(
            Number(RealConstToken(2.0, Position(1, 6))),
            SubtractionToken(Position(1, 10)),
            Number(IntegerConstToken(1, Position(1, 12)))
          ),
          MultiplicationToken(Position(1, 15)),
          Number(IntegerConstToken(2, Position(1, 17)))
        ),
        AssignToken(Position(1, 3))
      )
    )
  }
  test("Parse multiplication") {
    val parser = new Parser("2 * 3")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        Number(IntegerConstToken(2, Position(1, 1))),
        MultiplicationToken(Position(1, 3)),
        Number(IntegerConstToken(3, Position(1, 5)))
      )
    )

  }
  test("Parse addition") {
    val parser = new Parser("13 + 10")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        Number(IntegerConstToken(13, Position(1, 1))),
        AdditionToken(Position(1, 4)),
        Number(IntegerConstToken(10, Position(1, 6)))
      )
    )

  }
  test("Parse division and subtraction") {
    val parser = new Parser("50 - 10 / 2")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        Number(IntegerConstToken(50, Position(1, 1))),
        SubtractionToken(Position(1, 4)),
        BinOp(
          Number(IntegerConstToken(10, Position(1, 6))),
          DivisionToken(Position(1, 9)),
          Number(IntegerConstToken(2, Position(1, 11)))
        )
      )
    )
  }
  test("Parse arbitrary number of unary operators") {
    val parser = new Parser("--+999")()
    val ast = parser.parse().children.head
    assert(
      ast == UnaryOp(
        SubtractionToken(Position(1, 1)),
        UnaryOp(
          SubtractionToken(Position(1, 2)),
          UnaryOp(AdditionToken(Position(1, 3)), Number(IntegerConstToken(999, Position(1, 4))))
        )
      )
    )
  }
  test("Parse boolean expression") {
    val parser = new Parser("true or false and not true")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        BooleanLiteral(TrueToken(Position(1, 1))),
        OrToken(Position(1, 6)),
        BinOp(
          BooleanLiteral(FalseToken(Position(1, 9))),
          AndToken(Position(1, 15)),
          UnaryOp(NotToken(Position(1, 19)), BooleanLiteral(TrueToken(Position(1, 23))))
        )
      )
    )
  }
  test("Parser arithmetic boolean expression") {
    val parser = new Parser("1 + 9 > 2 / 4.2")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        BinOp(
          Number(IntegerConstToken(1, Position(1, 1))),
          AdditionToken(Position(1, 3)),
          Number(IntegerConstToken(9, Position(1, 5)))
        ),
        GreaterToken(Position(1, 7)),
        BinOp(
          Number(IntegerConstToken(2, Position(1, 9))),
          DivisionToken(Position(1, 11)),
          Number(RealConstToken(4.2, Position(1, 13)))
        )
      )
    )
  }
  test("Parser complex arithmetic boolean expression") {
    val parser = new Parser("x + 1 > 3 or y * 3 == 4")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        BinOp(
          BinOp(
            VarAST(IdToken("x", Position(1, 1))),
            AdditionToken(Position(1, 3)),
            Number(IntegerConstToken(1, Position(1, 5)))
          ),
          GreaterToken(Position(1, 7)),
          Number(IntegerConstToken(3, Position(1, 9)))
        ),
        OrToken(Position(1, 11)),
        BinOp(
          BinOp(
            VarAST(IdToken("y", Position(1, 14))),
            MultiplicationToken(Position(1, 16)),
            Number(IntegerConstToken(3, Position(1, 18)))
          ),
          EqualsToken(Position(1, 20)),
          Number(IntegerConstToken(4, Position(1, 23)))
        )
      )
    )
  }
  test("Parse multiple statements") {
    val parser = Parser.fromResource("parser/multipleStatements.intp")
    val ast = parser.parse().children
    assert(
      ast == List(
        AssignAST(
          IdToken("x", Position(1, 1)),
          BinOp(
            Number(IntegerConstToken(1, Position(1, 5))),
            MultiplicationToken(Position(1, 6)),
            Number(IntegerConstToken(2, Position(1, 7)))
          ),
          AssignToken(Position(1, 3))
        ),
        AssignAST(
          IdToken("y", Position(2, 1)),
          BinOp(
            Number(IntegerConstToken(2, Position(2, 5))),
            AdditionToken(Position(2, 7)),
            VarAST(IdToken("x", Position(2, 9)))
          ),
          AssignToken(Position(2, 3))
        ),
        AssignAST(
          IdToken("abcd", Position(3, 1)),
          ArrayLiteral(
            List(
              BinOp(
                Number(IntegerConstToken(2, Position(3, 9))),
                MultiplicationToken(Position(3, 11)),
                Number(IntegerConstToken(3, Position(3, 13)))
              ),
              Number(RealConstToken(1.3, Position(3, 16))),
              BinOp(
                VarAST(IdToken("y", Position(3, 21))),
                MultiplicationToken(Position(3, 23)),
                VarAST(IdToken("x", Position(3, 25)))
              ),
              BinOp(
                BooleanLiteral(TrueToken(Position(3, 28))),
                OrToken(Position(3, 33)),
                BooleanLiteral(FalseToken(Position(3, 36)))
              )
            ),
            LSquareBracketToken(Position(3, 8))
          ),
          AssignToken(Position(3, 6))
        ),
        BinOp(
          VarAST(IdToken("abcd", Position(4, 1))),
          MultiplicationToken(Position(4, 6)),
          Number(IntegerConstToken(2, Position(4, 8)))
        ),
        FunctionCall(
          VarAST(IdToken("f", Position(5, 1))),
          List(
            VarAST(IdToken("abcd", Position(5, 3))),
            BinOp(
              ArrayAccess(
                VarAST(IdToken("g", Position(5, 9))),
                Number(IntegerConstToken(1, Position(5, 11)))
              ),
              MultiplicationToken(Position(5, 14)),
              ArrayAccess(
                VarAST(IdToken("h", Position(5, 16))),
                Number(IntegerConstToken(2, Position(5, 18)))
              )
            )
          )
        )
      )
    )
  }
  test("Parse functions call") {
    val parser = new Parser("f(3, false)\ng()")()
    val ast = parser.parse().children
    assert(
      ast == List(
        FunctionCall(
          VarAST(IdToken("f", Position(1, 1))),
          List(
            Number(IntegerConstToken(3, Position(1, 3))),
            BooleanLiteral(FalseToken(Position(1, 6)))
          )
        ),
        FunctionCall(VarAST(IdToken("g", Position(2, 1))), List())
      )
    )
  }
  test("Parser array literal") {
    val parser = new Parser("[1, 2, 3]")()
    val ast = parser.parse().children
    assert(
      ast == List(
        ArrayLiteral(
          List(
            Number(IntegerConstToken(1, Position(1, 2))),
            Number(IntegerConstToken(2, Position(1, 5))),
            Number(IntegerConstToken(3, Position(1, 8)))
          ),
          LSquareBracketToken(Position(1, 1))
        )
      )
    )
  }
  test("Parse array indexing") {
    val parser = new Parser("x[0]")()
    val ast = parser.parse().children.head
    assert(
      ast == ArrayAccess(
        VarAST(IdToken("x", Position(1, 1))),
        Number(IntegerConstToken(0, Position(1, 3)))
      )
    )
  }
  test("Parse assignment for array indexed object") {
    val parser = new Parser("v[1] = 3")()
    val ast = parser.parse().children.head
    assert(
      ast == ArrayAssignAST(
        VarAST(IdToken("v", Position(1, 1))),
        Number(IntegerConstToken(1, Position(1, 3))),
        Number(IntegerConstToken(3, Position(1, 8)))
      )
    )
  }
  test("Parse multiple dimension array literal") {
    val parser = new Parser("[[1, 2], [3, 4]]")()
    val ast = parser.parse().children.head
    assert(
      ast == ArrayLiteral(
        List(
          ArrayLiteral(
            List(
              Number(IntegerConstToken(1, Position(1, 3))),
              Number(IntegerConstToken(2, Position(1, 6)))
            ),
            LSquareBracketToken(Position(1, 2))
          ),
          ArrayLiteral(
            List(
              Number(IntegerConstToken(3, Position(1, 11))),
              Number(IntegerConstToken(4, Position(1, 14)))
            ),
            LSquareBracketToken(Position(1, 10))
          )
        ),
        LSquareBracketToken(Position(1, 1))
      )
    )
  }
  test("Parse multiple dimension array indexing") {
    val parser = new Parser("a[0][2]")()
    val ast = parser.parse().children.head
    assert(
      ast == ArrayAccess(
        ArrayAccess(
          VarAST(IdToken("a", Position(1, 1))),
          Number(IntegerConstToken(0, Position(1, 3)))
        ),
        Number(IntegerConstToken(2, Position(1, 6)))
      )
    )
  }
  test("Parser chain function calls") {
    val parser = new Parser("f(1)(2)")()
    val ast = parser.parse().children.head
    assert(
      ast == FunctionCall(
        FunctionCall(
          VarAST(IdToken("f", Position(1, 1))),
          List(Number(IntegerConstToken(1, Position(1, 3))))
        ),
        List(Number(IntegerConstToken(2, Position(1, 6))))
      )
    )

  }
  test("Parse function definition") {
    val parser = Parser.fromResource("parser/functionDefinition.intp")
    val ast = parser.parse().children
    assert(
      ast == List(
        AssignAST(
          IdToken("f", Position(1, 1)),
          FunctionLiteral(
            List(IdToken("a", Position(1, 10)), IdToken("b", Position(1, 13))),
            Program(
              List(
                AssignAST(
                  IdToken("x", Position(2, 3)),
                  BinOp(
                    VarAST(IdToken("a", Position(2, 7))),
                    MultiplicationToken(Position(2, 9)),
                    Number(IntegerConstToken(2, Position(2, 11)))
                  ),
                  AssignToken(Position(2, 5))
                ),
                AssignAST(
                  IdToken("var", Position(3, 3)),
                  ArrayLiteral(
                    List(
                      VarAST(IdToken("a", Position(3, 10))),
                      VarAST(IdToken("b", Position(3, 13))),
                      VarAST(IdToken("x", Position(3, 16)))
                    ),
                    LSquareBracketToken(Position(3, 9))
                  ),
                  AssignToken(Position(3, 7))
                )
              )
            ),
            FuncToken(Position(1, 5))
          ),
          AssignToken(Position(1, 3))
        )
      )
    )
  }
  test("Parse character assignment") {
    val parser = new Parser("c = 'a'")()
    val ast = parser.parse().children.head
    assert(
      ast == AssignAST(
        IdToken("c", Position(1, 1)),
        CharLiteral(CharToken('a', Position(1, 6))),
        AssignToken(Position(1, 3))
      )
    )
  }
  test("Parse string assignment") {
    val parser = new Parser("s = \"text\"")()
    val ast = parser.parse().children.head
    assert(
      ast == AssignAST(
        IdToken("s", Position(1, 1)),
        ArrayLiteral(
          List(
            CharLiteral(CharToken('t', Position(1, 6))),
            CharLiteral(CharToken('e', Position(1, 6))),
            CharLiteral(CharToken('x', Position(1, 6))),
            CharLiteral(CharToken('t', Position(1, 6)))
          ),
          LSquareBracketToken(Position(1, 5))
        ),
        AssignToken(Position(1, 3))
      )
    )
  }
  test("Parse if statement") {
    val parser = Parser.fromResource("parser/if.intp")
    val ast = parser.parse().children.head
    assert(
      ast == IfAST(
        BooleanLiteral(FalseToken(Position(1, 4))),
        Program(List(FunctionCall(VarAST(IdToken("f", Position(2, 3))), List()))),
        None,
        IfToken(Position(1, 1))
      )
    )
  }
  test("Parse if-else statement") {
    val parser = Parser.fromResource("parser/if-else.intp")
    val ast = parser.parse().children.head
    assert(
      ast == IfAST(
        BinOp(
          VarAST(IdToken("x", Position(1, 4))),
          EqualsToken(Position(1, 6)),
          Number(IntegerConstToken(2, Position(1, 9)))
        ),
        Program(
          List(
            AssignAST(
              IdToken("c", Position(2, 3)),
              BooleanLiteral(TrueToken(Position(2, 7))),
              AssignToken(Position(2, 5))
            )
          )
        ),
        Some(
          Program(
            List(
              AssignAST(
                IdToken("c", Position(4, 3)),
                BooleanLiteral(FalseToken(Position(4, 7))),
                AssignToken(Position(4, 5))
              )
            )
          )
        ),
        IfToken(Position(1, 1))
      )
    )
  }
  test("Parse while statement") {
    val parser = Parser.fromResource("parser/while.intp")
    val ast = parser.parse().children.head
    assert(
      ast == WhileAST(
        BooleanLiteral(TrueToken(Position(1, 7))),
        Program(
          List(
            AssignAST(
              IdToken("x", Position(2, 3)),
              BinOp(
                VarAST(IdToken("x", Position(2, 7))),
                AdditionToken(Position(2, 9)),
                Number(IntegerConstToken(1, Position(2, 11)))
              ),
              AssignToken(Position(2, 5))
            )
          )
        ),
        WhileToken(Position(1, 1))
      )
    )
  }
  test("Parse function definition inside another function") {
    val parser = Parser.fromResource("parser/nestedFunctions.intp")
    val ast = parser.parse().children.head
    assert(
      ast == AssignAST(
        IdToken("f", Position(1, 1)),
        FunctionLiteral(
          List(IdToken("a", Position(1, 10)), IdToken("b", Position(1, 13))),
          Program(
            List(
              AssignAST(
                IdToken("g", Position(2, 3)),
                FunctionLiteral(
                  List(IdToken("c", Position(2, 12)), IdToken("d", Position(2, 15))),
                  Program(
                    List(
                      FunctionCall(
                        VarAST(IdToken("f", Position(3, 5))),
                        List(
                          VarAST(IdToken("c", Position(3, 7))),
                          VarAST(IdToken("d", Position(3, 10)))
                        )
                      )
                    )
                  ),
                  FuncToken(Position(2, 7))
                ),
                AssignToken(Position(2, 5))
              ),
              FunctionCall(
                VarAST(IdToken("g", Position(5, 3))),
                List(VarAST(IdToken("a", Position(5, 5))), VarAST(IdToken("b", Position(5, 8))))
              )
            )
          ),
          FuncToken(Position(1, 5))
        ),
        AssignToken(Position(1, 3))
      )
    )
  }
  test("Parse return statement") {
    val parser = Parser.fromResource("parser/return.intp")
    val ast = parser.parse().children.head
    assert(
      ast == AssignAST(
        IdToken("f", Position(1, 1)),
        FunctionLiteral(
          List(IdToken("a", Position(1, 10))),
          Program(
            List(
              IfAST(
                BinOp(
                  VarAST(IdToken("a", Position(2, 6))),
                  GreaterToken(Position(2, 8)),
                  Number(IntegerConstToken(10, Position(2, 10)))
                ),
                Program(
                  List(
                    ReturnAST(
                      BooleanLiteral(TrueToken(Position(3, 12))),
                      ReturnToken(Position(3, 5))
                    )
                  )
                ),
                None,
                IfToken(Position(2, 3))
              ),
              ReturnAST(BooleanLiteral(FalseToken(Position(5, 10))), ReturnToken(Position(5, 3)))
            )
          ),
          FuncToken(Position(1, 5))
        ),
        AssignToken(Position(1, 3))
      )
    )
  }
  test("Parse not equals") {
    val parser = new Parser("true != false")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        BooleanLiteral(TrueToken(Position(1, 1))),
        NotEqualsToken(Position(1, 6)),
        BooleanLiteral(FalseToken(Position(1, 9)))
      )
    )
  }
  test("Parse empty program") {
    val parser = new Parser("")()
    val ast = parser.parse()
    assert(
      ast == Program(List())
    )
  }
  test("Parse modulo operator") {
    val parser = new Parser("10 % 3")()
    val ast = parser.parse().children.head
    assert(
      ast == BinOp(
        Number(IntegerConstToken(10, Position(1, 1))),
        ModuloToken(Position(1, 4)),
        Number(IntegerConstToken(3, Position(1, 6)))
      )
    )
  }
  test("Parse source code with comments") {
    val parser = Parser.fromResource("parser/comments.intp")
    val ast = parser.parse()
    assert(
      ast == Program(
        List(
          AssignAST(
            IdToken("x", Position(2, 0)),
            ArrayLiteral(
              List(
                CharLiteral(CharToken('t', Position(2, 5))),
                CharLiteral(CharToken('e', Position(2, 5))),
                CharLiteral(CharToken('s', Position(2, 5))),
                CharLiteral(CharToken('t', Position(2, 5)))
              ),
              LSquareBracketToken(Position(2, 4))
            ),
            AssignToken(Position(2, 2))
          ),
          AssignAST(
            IdToken("y", Position(4, 0)),
            BinOp(
              VarAST(IdToken("x", Position(4, 4))),
              AdditionToken(Position(4, 6)),
              ArrayLiteral(
                List(
                  CharLiteral(CharToken(' ', Position(4, 9))),
                  CharLiteral(CharToken('.', Position(4, 9)))
                ),
                LSquareBracketToken(Position(4, 8))
              )
            ),
            AssignToken(Position(4, 2))
          )
        )
      )
    )
  }
  test("Parse else-if statement") {
    val parser = Parser.fromResource("parser/else-if.intp")
    val ast = parser.parse()
    assert(
      ast == Program(
        List(
          IfAST(
            BooleanLiteral(FalseToken(Position(1, 4))),
            Program(List()),
            Some(
              IfAST(
                BooleanLiteral(TrueToken(Position(3, 11))),
                Program(List()),
                None,
                IfToken(Position(3, 8))
              )
            ),
            IfToken(Position(1, 1))
          )
        )
      )
    )
  }
  test("Parse if-then-else") {
    val parser = Parser.fromResource("parser/if-then-else.intp")
    val ast = parser.parse()
    Program(
      List(
        AssignAST(
          IdToken("x", Position(1, 1)),
          Number(IntegerConstToken(10, Position(1, 5))),
          AssignToken(Position(1, 3))
        ),
        AssignAST(
          IdToken("y", Position(2, 1)),
          IfAST(
            BinOp(
              VarAST(IdToken("x", Position(2, 8))),
              LessToken(Position(2, 10)),
              Number(IntegerConstToken(5, Position(2, 12)))
            ),
            Number(IntegerConstToken(11, Position(2, 19))),
            Some(Number(IntegerConstToken(12, Position(2, 27)))),
            IfToken(Position(2, 5))
          ),
          AssignToken(Position(2, 3))
        )
      )
    )
  }
  test("Parse function call from array") {
    val parser = Parser.fromResource("parser/functionCallFromArray.intp")
    val ast = parser.parse()
    assert(ast ==
      Program(List(AssignAST(IdToken("x", Position(1, 1)),
        ArrayLiteral(List(FunctionLiteral(List(IdToken("x", Position(1, 11))),
          Program(List(ReturnAST(VarAST(IdToken("x", Position(1, 21))),
            ReturnToken(Position(1, 14))))), FuncToken(Position(1, 6)))),
          LSquareBracketToken(Position(1, 5))), AssignToken(Position(1, 3))),
        BuiltinFunctionCall(IdToken("println", Position(2, 1)),
          List(FunctionCall(ArrayAccess(VarAST(IdToken("x", Position(2, 9))),
            Number(IntegerConstToken(0, Position(2, 11)))),
            List(ArrayLiteral(List(CharLiteral(CharToken('t', Position(2, 15))),
              CharLiteral(CharToken('e', Position(2, 15))),
              CharLiteral(CharToken('s', Position(2, 15))),
              CharLiteral(CharToken('t', Position(2, 15)))),
              LSquareBracketToken(Position(2, 14)))))))))
    )
  }
  test("Parse indexing of array returned from function") {
    val parser = Parser.fromResource("parser/arrayIndexingFromFunction.intp")
    val ast = parser.parse()
    assert(ast ==
      Program(List(AssignAST(IdToken("f", Position(1, 1)), FunctionLiteral(List(),
        Program(List(ReturnAST(ArrayLiteral(List(Number(IntegerConstToken(1, Position(2, 11))),
          Number(IntegerConstToken(2, Position(2, 14))),
          Number(IntegerConstToken(15, Position(2, 17)))),
          LSquareBracketToken(Position(2, 10))), ReturnToken(Position(2, 3))))), FuncToken(Position(1, 5))), AssignToken(Position(1, 3))),
        BuiltinFunctionCall(IdToken("println", Position(4, 1)),
          List(ArrayAccess(FunctionCall(VarAST(IdToken("f", Position(4, 9))), List()),
            Number(IntegerConstToken(2, Position(4, 13))))))))
    )
  }
  test("Parse object literal") {
    val parser = Parser.fromResource("parser/objectLiteral.intp")
    val ast = parser.parse()
    assert(ast ==
      Program(
        List(AssignAST(IdToken("person", Position(1, 1)),
          ObjectLiteral(Map(IdToken("name", Position(1, 11)) -> ArrayLiteral(
            List(CharLiteral(CharToken('J', Position(1, 19))),
              CharLiteral(CharToken('o', Position(1, 19))),
              CharLiteral(CharToken('h', Position(1, 19))),
              CharLiteral(CharToken('n', Position(1, 19)))),
            LSquareBracketToken(Position(1, 18))),
            IdToken("surname", Position(1, 26)) -> ArrayLiteral(
              List(CharLiteral(CharToken('S', Position(1, 37))),
                CharLiteral(CharToken('m', Position(1, 37))),
                CharLiteral(CharToken('i', Position(1, 37))),
                CharLiteral(CharToken('t', Position(1, 37))),
                CharLiteral(CharToken('h', Position(1, 37)))),
              LSquareBracketToken(Position(1, 36)))),
            LCurlyBracketToken(Position(1, 10))), AssignToken(Position(1, 8)))))
    )
  }
  test("Object literal property access") {
    val parser = Parser.fromResource("parser/propertyAccess.intp")
    val ast = parser.parse()
    assert(ast ==
      Program(List(AssignAST(
        IdToken("obj", Position(1, 1)),
        ObjectLiteral(Map(IdToken("x", Position(1, 8)) -> Number(
          IntegerConstToken(1, Position(1, 12))), IdToken("z", Position(1, 22)) ->
          Number(IntegerConstToken(3, Position(1, 26))), IdToken("y", Position(1, 15)) ->
          Number(IntegerConstToken(2, Position(1, 19)))), LCurlyBracketToken(Position(1, 7))),
        AssignToken(Position(1, 5))),
        AssignAST(IdToken("f", Position(2, 1)), PropertyAccess(VarAST(IdToken("obj", Position(2, 5))),
          IdToken("y", Position(2, 9))), AssignToken(Position(2, 3)))))
    )
  }
}

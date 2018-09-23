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
    val parser = new Parser("val x = (2.0 - 1) * 2")()
    val ast = parser.parse().children.head
    assert(
      ast == ValDefAST(
        IdToken("x", Position(1, 5)),
        BinOp(
          BinOp(
            Number(RealConstToken(2.0, Position(1, 10))),
            SubtractionToken(Position(1, 14)),
            Number(IntegerConstToken(1, Position(1, 16)))
          ),
          MultiplicationToken(Position(1, 19)),
          Number(IntegerConstToken(2, Position(1, 21)))
        ),
        inLoopBody = false,
        ValToken(Position(1, 1))
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
        ValDefAST(
          IdToken("x", Position(1, 5)),
          BinOp(
            Number(IntegerConstToken(1, Position(1, 9))),
            MultiplicationToken(Position(1, 10)),
            Number(IntegerConstToken(2, Position(1, 11)))
          ),
          inLoopBody = false,
          ValToken(Position(1, 1))
        ),
        ValDefAST(
          IdToken("y", Position(2, 5)),
          BinOp(
            Number(IntegerConstToken(2, Position(2, 9))),
            AdditionToken(Position(2, 11)),
            VarAST(IdToken("x", Position(2, 13)))
          ),
          inLoopBody = false,
          ValToken(Position(2, 1))
        ),
        ValDefAST(
          IdToken("abcd", Position(3, 5)),
          ArrayLiteral(
            List(
              BinOp(
                Number(IntegerConstToken(2, Position(3, 13))),
                MultiplicationToken(Position(3, 15)),
                Number(IntegerConstToken(3, Position(3, 17)))
              ),
              Number(RealConstToken(1.3, Position(3, 20))),
              BinOp(
                VarAST(IdToken("y", Position(3, 25))),
                MultiplicationToken(Position(3, 27)),
                VarAST(IdToken("x", Position(3, 29)))
              ),
              BinOp(
                BooleanLiteral(TrueToken(Position(3, 32))),
                OrToken(Position(3, 37)),
                BooleanLiteral(FalseToken(Position(3, 40)))
              )
            ),
            LSquareBracketToken(Position(3, 12))
          ),
          inLoopBody = false,
          ValToken(Position(3, 1))
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
        ValDefAST(
          IdToken("f", Position(1, 5)),
          FunctionLiteral(
            List(IdToken("a", Position(1, 14)), IdToken("b", Position(1, 17))),
            Program(
              List(
                ValDefAST(
                  IdToken("x", Position(2, 7)),
                  BinOp(
                    VarAST(IdToken("a", Position(2, 11))),
                    MultiplicationToken(Position(2, 13)),
                    Number(IntegerConstToken(2, Position(2, 15)))
                  ),
                  inLoopBody = false,
                  ValToken(Position(2, 3))
                ),
                VarDefAST(
                  IdToken("arr", Position(3, 7)),
                  ArrayLiteral(
                    List(
                      VarAST(IdToken("a", Position(3, 14))),
                      VarAST(IdToken("b", Position(3, 17))),
                      VarAST(IdToken("x", Position(3, 20)))
                    ),
                    LSquareBracketToken(Position(3, 13))
                  ),
                  inLoopBody = false,
                  VarToken(Position(3, 3))
                )
              )
            ),
            FuncToken(Position(1, 9))
          ),
          inLoopBody = false,
          ValToken(Position(1, 1))
        )
      )
    )
  }
  test("Parse character assignment") {
    val parser = new Parser("val c = 'a'")()
    val ast = parser.parse().children.head
    assert(
      ast == ValDefAST(
        IdToken("c", Position(1, 5)),
        CharLiteral(CharToken('a', Position(1, 10))),
        inLoopBody = false,
        ValToken(Position(1, 1))
      )
    )
  }
  test("Parse string assignment") {
    val parser = new Parser("val s = \"text\"")()
    val ast = parser.parse().children.head
    assert(
      ast == ValDefAST(
        IdToken("s", Position(1, 5)),
        ArrayLiteral(
          List(
            CharLiteral(CharToken('t', Position(1, 10))),
            CharLiteral(CharToken('e', Position(1, 10))),
            CharLiteral(CharToken('x', Position(1, 10))),
            CharLiteral(CharToken('t', Position(1, 10)))
          ),
          LSquareBracketToken(Position(1, 9))
        ),
        inLoopBody = false,
        ValToken(Position(1, 1))
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
            ValDefAST(
              IdToken("c", Position(2, 7)),
              BooleanLiteral(TrueToken(Position(2, 11))),
              inLoopBody = false,
              ValToken(Position(2, 3))
            )
          )
        ),
        Some(
          Program(
            List(
              ValDefAST(
                IdToken("c", Position(4, 7)),
                BooleanLiteral(FalseToken(Position(4, 11))),
                inLoopBody = false,
                ValToken(Position(4, 3))
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
            VarDefAST(
              IdToken("x", Position(2, 7)),
              BinOp(
                VarAST(IdToken("x", Position(2, 11))),
                AdditionToken(Position(2, 13)),
                Number(IntegerConstToken(1, Position(2, 15)))
              ),
              inLoopBody = true,
              VarToken(Position(2, 3))
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
      ast == ValDefAST(
        IdToken("f", Position(1, 5)),
        FunctionLiteral(
          List(IdToken("a", Position(1, 14)), IdToken("b", Position(1, 17))),
          Program(
            List(
              ValDefAST(
                IdToken("g", Position(2, 7)),
                FunctionLiteral(
                  List(IdToken("c", Position(2, 16)), IdToken("d", Position(2, 19))),
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
                  FuncToken(Position(2, 11))
                ),
                inLoopBody = false,
                ValToken(Position(2, 3))
              ),
              FunctionCall(
                VarAST(IdToken("g", Position(5, 3))),
                List(VarAST(IdToken("a", Position(5, 5))), VarAST(IdToken("b", Position(5, 8))))
              )
            )
          ),
          FuncToken(Position(1, 9))
        ),
        inLoopBody = false,
        ValToken(Position(1, 1))
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
          ValDefAST(
            IdToken("x", Position(2, 4)),
            ArrayLiteral(
              List(
                CharLiteral(CharToken('t', Position(2, 9))),
                CharLiteral(CharToken('e', Position(2, 9))),
                CharLiteral(CharToken('s', Position(2, 9))),
                CharLiteral(CharToken('t', Position(2, 9)))
              ),
              LSquareBracketToken(Position(2, 8))
            ),
            inLoopBody = false,
            ValToken(Position(2, 0))
          ),
          ValDefAST(
            IdToken("y", Position(4, 4)),
            BinOp(
              VarAST(IdToken("x", Position(4, 8))),
              AdditionToken(Position(4, 10)),
              ArrayLiteral(
                List(
                  CharLiteral(CharToken(' ', Position(4, 13))),
                  CharLiteral(CharToken('.', Position(4, 13)))
                ),
                LSquareBracketToken(Position(4, 12))
              )
            ),
            inLoopBody = false,
            ValToken(Position(4, 0))
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
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("x", Position(1, 5)),
              ArrayLiteral(
                List(
                  FunctionLiteral(
                    List(IdToken("x", Position(1, 15))),
                    Program(
                      List(
                        ReturnAST(
                          VarAST(IdToken("x", Position(1, 25))),
                          ReturnToken(Position(1, 18))
                        )
                      )
                    ),
                    FuncToken(Position(1, 10))
                  )
                ),
                LSquareBracketToken(Position(1, 9))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            ),
            BuiltinFunctionCall(
              IdToken("println", Position(2, 1)),
              List(
                FunctionCall(
                  ArrayAccess(
                    VarAST(IdToken("x", Position(2, 9))),
                    Number(IntegerConstToken(0, Position(2, 11)))
                  ),
                  List(
                    ArrayLiteral(
                      List(
                        CharLiteral(CharToken('t', Position(2, 15))),
                        CharLiteral(CharToken('e', Position(2, 15))),
                        CharLiteral(CharToken('s', Position(2, 15))),
                        CharLiteral(CharToken('t', Position(2, 15)))
                      ),
                      LSquareBracketToken(Position(2, 14))
                    )
                  )
                )
              )
            )
          )
        )
    )
  }
  test("Parse indexing of array returned from function") {
    val parser = Parser.fromResource("parser/arrayIndexingFromFunction.intp")
    val ast = parser.parse()
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("f", Position(1, 5)),
              FunctionLiteral(
                List(),
                Program(
                  List(
                    ReturnAST(
                      ArrayLiteral(
                        List(
                          Number(IntegerConstToken(1, Position(2, 11))),
                          Number(IntegerConstToken(2, Position(2, 14))),
                          Number(IntegerConstToken(15, Position(2, 17)))
                        ),
                        LSquareBracketToken(Position(2, 10))
                      ),
                      ReturnToken(Position(2, 3))
                    )
                  )
                ),
                FuncToken(Position(1, 9))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            ),
            BuiltinFunctionCall(
              IdToken("println", Position(4, 1)),
              List(
                ArrayAccess(
                  FunctionCall(VarAST(IdToken("f", Position(4, 9))), List()),
                  Number(IntegerConstToken(2, Position(4, 13)))
                )
              )
            )
          )
        )
    )
  }
  test("Parse object literal") {
    val parser = Parser.fromResource("parser/objectLiteral.intp")
    val ast = parser.parse()
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("person", Position(1, 5)),
              ObjectLiteral(
                Map(
                  IdToken("name", Position(1, 15)) -> ArrayLiteral(
                    List(
                      CharLiteral(CharToken('J', Position(1, 23))),
                      CharLiteral(CharToken('o', Position(1, 23))),
                      CharLiteral(CharToken('h', Position(1, 23))),
                      CharLiteral(CharToken('n', Position(1, 23)))
                    ),
                    LSquareBracketToken(Position(1, 22))
                  ),
                  IdToken("surname", Position(1, 30)) -> ArrayLiteral(
                    List(
                      CharLiteral(CharToken('S', Position(1, 41))),
                      CharLiteral(CharToken('m', Position(1, 41))),
                      CharLiteral(CharToken('i', Position(1, 41))),
                      CharLiteral(CharToken('t', Position(1, 41))),
                      CharLiteral(CharToken('h', Position(1, 41)))
                    ),
                    LSquareBracketToken(Position(1, 40))
                  )
                ),
                LCurlyBracketToken(Position(1, 14))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            )
          )
        )
    )
  }
  test("Object literal property access") {
    val parser = Parser.fromResource("parser/propertyAccess.intp")
    val ast = parser.parse()
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("obj", Position(1, 5)),
              ObjectLiteral(
                Map(
                  IdToken("x", Position(1, 12)) -> Number(IntegerConstToken(1, Position(1, 16))),
                  IdToken("y", Position(1, 19)) -> Number(IntegerConstToken(2, Position(1, 23))),
                  IdToken("z", Position(1, 26)) -> Number(IntegerConstToken(3, Position(1, 30)))
                ),
                LCurlyBracketToken(Position(1, 11))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            ),
            ValDefAST(
              IdToken("f", Position(2, 5)),
              PropertyAccess(VarAST(IdToken("obj", Position(2, 9))), IdToken("y", Position(2, 13))),
              inLoopBody = false,
              ValToken(Position(2, 1))
            )
          )
        )
    )
  }
  test("Data keyword desugars to function") {
    val parser = Parser.fromResource("parser/data.intp")
    val ast = parser.parse()
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("Person", Position(1, 5)),
              FunctionLiteral(
                List(IdToken("name", Position(1, 19)), IdToken("surname", Position(1, 25))),
                Program(
                  List(
                    ReturnAST(
                      ObjectLiteral(
                        Map(
                          IdToken("name", Position(1, 19)) -> VarAST(
                            IdToken("name", Position(1, 19))
                          ),
                          IdToken("surname", Position(1, 25)) -> VarAST(
                            IdToken("surname", Position(1, 25))
                          )
                        ),
                        LCurlyBracketToken(Position(1, 14))
                      ),
                      ReturnToken(Position(1, 14))
                    )
                  )
                ),
                FuncToken(Position(1, 14))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            ),
            ValDefAST(
              IdToken("person", Position(2, 5)),
              FunctionCall(
                VarAST(IdToken("Person", Position(2, 14))),
                List(
                  ArrayLiteral(
                    List(
                      CharLiteral(CharToken('J', Position(2, 22))),
                      CharLiteral(CharToken('o', Position(2, 22))),
                      CharLiteral(CharToken('h', Position(2, 22))),
                      CharLiteral(CharToken('n', Position(2, 22)))
                    ),
                    LSquareBracketToken(Position(2, 21))
                  ),
                  ArrayLiteral(
                    List(
                      CharLiteral(CharToken('S', Position(2, 30))),
                      CharLiteral(CharToken('m', Position(2, 30))),
                      CharLiteral(CharToken('i', Position(2, 30))),
                      CharLiteral(CharToken('t', Position(2, 30))),
                      CharLiteral(CharToken('h', Position(2, 30)))
                    ),
                    LSquareBracketToken(Position(2, 29))
                  )
                )
              ),
              inLoopBody = false,
              ValToken(Position(2, 1))
            ),
            ValDefAST(
              IdToken("n", Position(3, 5)),
              PropertyAccess(
                VarAST(IdToken("person", Position(3, 9))),
                IdToken("name", Position(3, 16))
              ),
              inLoopBody = false,
              ValToken(Position(3, 1))
            ),
            ValDefAST(
              IdToken("s", Position(4, 5)),
              PropertyAccess(
                VarAST(IdToken("person", Position(4, 9))),
                IdToken("surname", Position(4, 16))
              ),
              inLoopBody = false,
              ValToken(Position(4, 1))
            )
          )
        )
    )
  }
  test("Property assignment") {
    val parser = Parser.fromResource("parser/propertyAssignment.intp")
    val ast = parser.parse()
    assert(
      ast ==
        Program(
          List(
            ValDefAST(
              IdToken("test", Position(1, 5)),
              ObjectLiteral(
                Map(IdToken("a", Position(1, 13)) -> CharLiteral(CharToken('f', Position(1, 18)))),
                LCurlyBracketToken(Position(1, 12))
              ),
              inLoopBody = false,
              ValToken(Position(1, 1))
            ),
            PropertyAssignAST(
              VarAST(IdToken("test", Position(2, 1))),
              IdToken("a", Position(2, 6)),
              CharLiteral(CharToken('x', Position(2, 11)))
            )
          )
        )
    )
  }
  test("Parse double comment") {
    val parser = Parser.fromResource("parser/comments2.intp")
    parser.parse()
  }
}

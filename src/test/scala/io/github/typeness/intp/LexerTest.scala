package io.github.typeness.intp

import org.scalatest.FunSuite

class LexerTest extends FunSuite {
  test("Tokenize all possible tokens") {
    val lexer = new Lexer(
      "+ * / - { } [ ] ( ) 1234 12.34 'f' \"\" ," +
        " < <= >= > == or while true false if else and not return break . import = identifier" +
        " func != % then")()
    assert(lexer.getNextToken == AdditionToken(Position(1, 1)))
    assert(lexer.getNextToken == MultiplicationToken(Position(1, 3)))
    assert(lexer.getNextToken == DivisionToken(Position(1, 5)))
    assert(lexer.getNextToken == SubtractionToken(Position(1, 7)))
    assert(lexer.getNextToken == LCurlyBracketToken(Position(1, 9)))
    assert(lexer.getNextToken == RCurlyBracketToken(Position(1, 11)))
    assert(lexer.getNextToken == LSquareBracketToken(Position(1, 13)))
    assert(lexer.getNextToken == RSquareBracketToken(Position(1, 15)))
    assert(lexer.getNextToken == LRoundBracketToken(Position(1, 17)))
    assert(lexer.getNextToken == RRoundBracketToken(Position(1, 19)))
    assert(lexer.getNextToken == IntegerConstToken(1234, Position(1, 21)))
    assert(lexer.getNextToken == RealConstToken(12.34, Position(1, 26)))
    assert(lexer.getNextToken == ApostropheToken(Position(1, 32)))
    assert(lexer.getNextToken == CharToken('f', Position(1, 33)))
    assert(lexer.getNextToken == ApostropheToken(Position(1, 34)))
    assert(lexer.getNextToken == QuotationToken(Position(1, 36)))
    assert(lexer.getNextToken == StringToken("", Position(1, 37)))
    assert(lexer.getNextToken == QuotationToken(Position(1, 37)))
    assert(lexer.getNextToken == CommaToken(Position(1, 39)))
    assert(lexer.getNextToken == LessToken(Position(1, 41)))
    assert(lexer.getNextToken == LessOrEqualsToken(Position(1, 43)))
    assert(lexer.getNextToken == GreaterOrEqualsToken(Position(1, 46)))
    assert(lexer.getNextToken == GreaterToken(Position(1, 49)))
    assert(lexer.getNextToken == EqualsToken(Position(1, 51)))
    assert(lexer.getNextToken == OrToken(Position(1, 54)))
    assert(lexer.getNextToken == WhileToken(Position(1, 57)))
    assert(lexer.getNextToken == TrueToken(Position(1, 63)))
    assert(lexer.getNextToken == FalseToken(Position(1, 68)))
    assert(lexer.getNextToken == IfToken(Position(1, 74)))
    assert(lexer.getNextToken == ElseToken(Position(1, 77)))
    assert(lexer.getNextToken == AndToken(Position(1, 82)))
    assert(lexer.getNextToken == NotToken(Position(1, 86)))
    assert(lexer.getNextToken == ReturnToken(Position(1, 90)))
    assert(lexer.getNextToken == BreakToken(Position(1, 97)))
    assert(lexer.getNextToken == DotToken(Position(1, 103)))
    assert(lexer.getNextToken == ImportToken(Position(1, 105)))
    assert(lexer.getNextToken == AssignToken(Position(1, 112)))
    assert(lexer.getNextToken == IdToken("identifier", Position(1, 114)))
    assert(lexer.getNextToken == FuncToken(Position(1, 125)))
    assert(lexer.getNextToken == NotEqualsToken(Position(1, 130)))
    assert(lexer.getNextToken == ModuloToken(Position(1, 133)))
    assert(lexer.getNextToken == ThenToken(Position(1, 135)))
    assert(lexer.getNextToken == EOFToken(Position(1, 138)))
  }
  test("Tokenize string with special character and id") {
    val lexer = new Lexer("\"this is some string with special \n char\" id")()
    assert(lexer.getNextToken == QuotationToken(Position(1, 1)))
    assert(
      lexer.getNextToken == StringToken(
        "this is some string with special \n char",
        Position(1, 2)))
    assert(lexer.getNextToken == QuotationToken(Position(1, 41)))
    assert(lexer.getNextToken == IdToken("id", Position(1, 43)))
    assert(lexer.getNextToken == EOFToken(Position(1, 44)))
  }
  test("Tokenize apostrophe character") {
    val lexer = new Lexer("'''")()
    assert(lexer.getNextToken == ApostropheToken(Position(1, 1)))
    assert(lexer.getNextToken == CharToken('\'', Position(1, 2)))
    assert(lexer.getNextToken == ApostropheToken(Position(1, 3)))
    assert(lexer.getNextToken == EOFToken(Position(1, 3)))
  }
  test("Tokenize ' string") {
    val lexer = new Lexer("\"'\"")()
    assert(lexer.getNextToken == QuotationToken(Position(1, 1)))
    assert(lexer.getNextToken == StringToken("'", Position(1, 2)))
    assert(lexer.getNextToken == QuotationToken(Position(1, 3)))
    assert(lexer.getNextToken == EOFToken(Position(1, 3)))
  }
  test("Tokenize empty input") {
    val lexer = new Lexer("")()
    assert(lexer.getNextToken == EOFToken(Position(1, 0)))
  }
  test("Do not tokenize comment") {
    val lexer = new Lexer("// do not tokenize")()
    assert(lexer.getNextToken == EOFToken(Position(2, -1)))
  }
}

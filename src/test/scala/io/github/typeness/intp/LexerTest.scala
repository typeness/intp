package io.github.typeness.intp

import org.scalatest.FunSuite

class LexerTest extends FunSuite {
  test("Tokenize all possible tokens") {
    val lexer = new Lexer("+ * / - { } [ ] ( ) 1234 12.34 'f' \"\" ," +
      " < <= >= > == or while true false if else and not return break . import = identifier" +
      " func !=")
    assert(lexer.getNextToken == AdditionToken)
    assert(lexer.getNextToken == MultiplicationToken)
    assert(lexer.getNextToken == DivisionToken)
    assert(lexer.getNextToken == SubtractionToken)
    assert(lexer.getNextToken == LCurlyBracketToken)
    assert(lexer.getNextToken == RCurlyBracketToken)
    assert(lexer.getNextToken == LSquareBracketToken)
    assert(lexer.getNextToken == RSquareBracketToken)
    assert(lexer.getNextToken == LRoundBracketToken)
    assert(lexer.getNextToken == RRoundBracketToken)
    assert(lexer.getNextToken == IntegerConstToken(1234))
    assert(lexer.getNextToken == RealConstToken(12.34))
    assert(lexer.getNextToken == ApostropheToken)
    assert(lexer.getNextToken == CharToken('f'))
    assert(lexer.getNextToken == ApostropheToken)
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == StringToken(""))
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == CommaToken)
    assert(lexer.getNextToken == LessToken)
    assert(lexer.getNextToken == LessOrEqualsToken)
    assert(lexer.getNextToken == GreaterOrEqualsToken)
    assert(lexer.getNextToken == GreaterToken)
    assert(lexer.getNextToken == EqualsToken)
    assert(lexer.getNextToken == OrToken)
    assert(lexer.getNextToken == WhileToken)
    assert(lexer.getNextToken == TrueToken)
    assert(lexer.getNextToken == FalseToken)
    assert(lexer.getNextToken == IfToken)
    assert(lexer.getNextToken == ElseToken)
    assert(lexer.getNextToken == AndToken)
    assert(lexer.getNextToken == NotToken)
    assert(lexer.getNextToken == ReturnToken)
    assert(lexer.getNextToken == BreakToken)
    assert(lexer.getNextToken == DotToken)
    assert(lexer.getNextToken == ImportToken)
    assert(lexer.getNextToken == AssignToken)
    assert(lexer.getNextToken == IdToken("identifier"))
    assert(lexer.getNextToken == FuncToken)
    assert(lexer.getNextToken == NotEqualsToken)
    assert(lexer.getNextToken == EOFToken)
  }
  test("Tokenize string with special character and id") {
    val lexer = new Lexer("\"this is some string with special \n char\" id")
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == StringToken("this is some string with special \n char"))
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == IdToken("id"))
    assert(lexer.getNextToken == EOFToken)
  }
  test("Tokenize apostrophe character") {
    val lexer = new Lexer("'''")
    assert(lexer.getNextToken == ApostropheToken)
    assert(lexer.getNextToken == CharToken('\''))
    assert(lexer.getNextToken == ApostropheToken)
    assert(lexer.getNextToken == EOFToken)
  }
  test("Tokenize ' string") {
    val lexer = new Lexer("\"'\"")
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == StringToken("'"))
    assert(lexer.getNextToken == QuotationToken)
    assert(lexer.getNextToken == EOFToken)
  }
}

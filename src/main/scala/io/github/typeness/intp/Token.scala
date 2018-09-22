package io.github.typeness.intp

case class Position(row: Int, col: Int)

sealed trait Token {
  def tokenType: TokenType

  def value: String

  def position: Position
}

sealed trait NumberToken extends Token

case class IntegerConstToken(intValue: Int, position: Position) extends NumberToken {
  override def tokenType: TokenType = INTEGER_CONST

  override def value: String = intValue.toString
}

case class RealConstToken(doubleValue: Double, position: Position) extends NumberToken {
  override def tokenType: TokenType = REAL_CONST

  override def value: String = doubleValue.toString
}

case class StringToken(value: String, position: Position) extends Token {
  override def tokenType: STRING.type = STRING
  override def toString: String = s"StringToken('$value',$position)"

}

case class CharToken(c: Char, position: Position) extends Token {
  override def tokenType: CHARACTER.type = CHARACTER

  override def value: String = c.toString
  override def toString: String = s"CharToken('$c',$position)"
}

case class SubtractionToken(position: Position) extends Token {
  override def tokenType: TokenType = MINUS

  override def value: String = "-"
}

case class AdditionToken(position: Position) extends Token {
  override def tokenType: TokenType = PLUS

  override def value: String = "+"
}

case class MultiplicationToken(position: Position) extends Token {
  override def tokenType: TokenType = MULTIPLICATION

  override def value: String = "*"
}

case class DivisionToken(position: Position) extends Token {
  override def tokenType: TokenType = DIV

  override def value: String = "/"
}

case class NotToken(position: Position) extends Token {
  override def tokenType: TokenType = NOT

  override def value: String = "not"
}

case class OrToken(position: Position) extends Token {
  override def tokenType: TokenType = OR

  override def value: String = "or"
}

case class AndToken(position: Position) extends Token {
  override def tokenType: TokenType = AND

  override def value: String = "and"
}

case class AssignToken(position: Position) extends Token {
  override def tokenType: TokenType = ASSIGN

  override def value: String = "="
}

case class GreaterToken(position: Position) extends Token {
  override def tokenType: TokenType = GREATER

  override def value: String = ">"
}

case class GreaterOrEqualsToken(position: Position) extends Token {
  override def tokenType: TokenType = GREATER_OR_EQUALS

  override def value: String = ">="
}

case class LessToken(position: Position) extends Token {
  override def tokenType: TokenType = LESS

  override def value: String = "<"
}

case class LessOrEqualsToken(position: Position) extends Token {
  override def tokenType: TokenType = LESS_OR_EQUALS

  override def value: String = "<="
}

case class EqualsToken(position: Position) extends Token {
  override def tokenType: TokenType = EQUALS

  override def value: String = "=="
}

case class NotEqualsToken(position: Position) extends Token {
  override def tokenType: TokenType = NOT_EQUALS

  override def value: String = "!="
}

case class ModuloToken(position: Position) extends Token {
  override def tokenType: TokenType = MODULO

  override def value: String = "%"
}

case class LRoundBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = L_ROUND_BRACKET

  override def value: String = "("
}

case class RRoundBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = R_ROUND_BRACKET

  override def value: String = ")"
}

case class LSquareBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = L_SQUARE_BRACKET

  override def value: String = "["
}

case class RSquareBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = R_SQUARE_BRACKET

  override def value: String = "]"
}

case class RCurlyBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = R_CURLY_BRACKET

  override def value: String = "}"
}

case class LCurlyBracketToken(position: Position) extends Token {
  override def tokenType: TokenType = L_CURLY_BRACKET

  override def value: String = "{"
}

case class EOFToken(position: Position) extends Token {
  override def tokenType: TokenType = EOF

  override def value: String = "EOF"
}

case class IdToken(value: String, position: Position) extends Token {
  override def tokenType: TokenType = ID

  override def toString: String =
    "IdToken(" + "\"" + value + "\", " + position.toString + ")"
}

case class CommaToken(position: Position) extends Token {
  override def tokenType: TokenType = COMMA

  override def value: String = ","
}

case class ApostropheToken(position: Position) extends Token {
  override def tokenType: TokenType = APOSTROPHE

  override def value: String = "'"
}

case class DotToken(position: Position) extends Token {
  override def tokenType: TokenType = DOT

  override def value: String = "."
}

case class QuotationToken(position: Position) extends Token {
  override def tokenType: TokenType = QUOTATION

  override def value: String = "\""
}

case class SemicolonToken(position: Position) extends Token {
  override def tokenType: TokenType = SEMICOLON

  override def value: String = ";"
}

case class ImportToken(position: Position) extends Token {
  override def tokenType: TokenType = IMPORT

  override def value: String = "import"
}

case class WhileToken(position: Position) extends Token {
  override def tokenType: TokenType = WHILE

  override def value: String = "while"
}

case class IfToken(position: Position) extends Token {
  override def tokenType: TokenType = IF

  override def value: String = "if"
}

case class ElseToken(position: Position) extends Token {
  override def tokenType: TokenType = ELSE

  override def value: String = "else"
}

sealed trait TrueOrElseToken extends Token

case class TrueToken(position: Position) extends TrueOrElseToken {
  override def tokenType: TokenType = TRUE

  override def value: String = "true"
}

case class FalseToken(position: Position) extends TrueOrElseToken {
  override def tokenType: TokenType = FALSE

  override def value: String = "false"
}

case class BreakToken(position: Position) extends Token {
  override def tokenType: TokenType = BREAK

  override def value: String = "break"
}

case class ReturnToken(position: Position) extends Token {
  override def tokenType: TokenType = RETURN

  override def value: String = "return"
}

case class FuncToken(position: Position) extends Token {
  override def tokenType: TokenType = FUNC

  override def value: String = "func"
}

case class ThenToken(position: Position) extends Token {
  override def tokenType: TokenType = THEN

  override def value: String = "then"
}

case class DataToken(position: Position) extends Token {
  override def tokenType: DATA.type = DATA

  override def value = "data"
}

case class ForToken(position: Position) extends Token {
  override def tokenType: TokenType = FOR

  override def value: String = "for"
}

case class ValToken(position: Position) extends Token {
  override def tokenType: TokenType = VAL

  override def value: String = "val"
}

case class VarToken(position: Position) extends Token {
  override def tokenType: TokenType = VAR

  override def value: String = "var"
}

case class CompoundAdditionToken(position: Position) extends Token {
  override def tokenType: TokenType = COMPOUND_PLUS

  override def value: String = "+="
}

case class CompoundSubtractionToken(position: Position) extends Token {
  override def tokenType: TokenType = COMPOUND_MINUS

  override def value: String = "-="
}

case class CompoundMultiplicationToken(position: Position) extends Token {
  override def tokenType: TokenType = COMPOUND_MULTIPLICATION

  override def value: String = "*="
}

case class CompoundDivisionToken(position: Position) extends Token {
  override def tokenType: TokenType = COMPOUND_DIV

  override def value: String = "/="
}

case class CompoundModuloToken(position: Position) extends Token {
  override def tokenType: TokenType = COMPOUND_MODULO

  override def value: String = "%="
}

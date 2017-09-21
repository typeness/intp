package io.github.typeness.intp

sealed trait Token {
  def tokenType: TokenType

  def value: String
}

sealed trait NumberToken extends Token

case class IntegerConstToken(intValue: Int) extends NumberToken {
  override def tokenType: TokenType = INTEGER_CONST

  override def value: String = intValue.toString
}

case class RealConstToken(doubleValue: Double) extends NumberToken {
  override def tokenType: TokenType = REAL_CONST

  override def value: String = doubleValue.toString
}

case class StringToken(value: String) extends Token {
  override def tokenType: STRING.type = STRING
}

case class CharToken(c: Char) extends Token {
  override def tokenType: CHARACTER.type = CHARACTER

  override def value: String = c.toString
}

case object SubtractionToken extends Token {
  override def tokenType: TokenType = MINUS

  override def value: String = "-"
}

case object AdditionToken extends Token {
  override def tokenType: TokenType = PLUS

  override def value: String = "+"
}

case object MultiplicationToken extends Token {
  override def tokenType: TokenType = MULTIPLICATION

  override def value: String = "*"
}

case object DivisionToken extends Token {
  override def tokenType: TokenType = DIV

  override def value: String = "/"
}

case object NotToken extends Token {
  override def tokenType: TokenType = NOT

  override def value: String = "not"
}

case object OrToken extends Token {
  override def tokenType: TokenType = OR

  override def value: String = "or"
}

case object AndToken extends Token {
  override def tokenType: TokenType = AND

  override def value: String = "and"
}

case object AssignToken extends Token {
  override def tokenType: TokenType = ASSIGN

  override def value: String = "="
}

case object GreaterToken extends Token {
  override def tokenType: TokenType = GREATER

  override def value: String = ">"
}

case object GreaterOrEqualsToken extends Token {
  override def tokenType: TokenType = GREATER_OR_EQUALS

  override def value: String = ">="
}

case object LessToken extends Token {
  override def tokenType: TokenType = LESS

  override def value: String = "<"
}

case object LessOrEqualsToken extends Token {
  override def tokenType: TokenType = LESS_OR_EQUALS

  override def value: String = "<="
}

case object EqualsToken extends Token {
  override def tokenType: TokenType = EQUALS

  override def value: String = "=="
}

case object NotEqualsToken extends Token {
  override def tokenType: NOT_EQUALS.type = NOT_EQUALS

  override def value: String = "!="
}


case object LRoundBracketToken extends Token {
  override def tokenType: TokenType = L_ROUND_BRACKET

  override def value: String = "("
}

case object RRoundBracketToken extends Token {
  override def tokenType: TokenType = R_ROUND_BRACKET

  override def value: String = ")"
}

case object LSquareBracketToken extends Token {
  override def tokenType: TokenType = L_SQUARE_BRACKET

  override def value: String = "["
}

case object RSquareBracketToken extends Token {
  override def tokenType: TokenType = R_SQUARE_BRACKET

  override def value: String = "]"
}

case object RCurlyBracketToken extends Token {
  override def tokenType: TokenType = R_CURLY_BRACKET

  override def value: String = "}"
}

case object LCurlyBracketToken extends Token {
  override def tokenType: TokenType = L_CURLY_BRACKET

  override def value: String = "{"
}

case object EOFToken extends Token {
  override def tokenType: TokenType = EOF

  override def value: String = "EOF"
}

case class IdToken(value: String) extends Token {
  override def tokenType: TokenType = ID
}

case object CommaToken extends Token {
  override def tokenType: TokenType = COMMA

  override def value: String = ","
}

case object ApostropheToken extends Token {
  override def tokenType: TokenType = APOSTROPHE

  override def value: String = "'"
}

case object DotToken extends Token {
  override def tokenType: TokenType = DOT

  override def value: String = "."
}

case object QuotationToken extends Token {
  override def tokenType: TokenType = QUOTATION

  override def value: String = "\""
}

case object ImportToken extends Token {
  override def tokenType: TokenType = IMPORT

  override def value: String = "import"
}

case object WhileToken extends Token {
  override def tokenType: TokenType = WHILE

  override def value: String = "while"
}

case object IfToken extends Token {
  override def tokenType: TokenType = IF

  override def value: String = "if"
}

case object ElseToken extends Token {
  override def tokenType: TokenType = ELSE

  override def value: String = "else"
}

sealed trait TrueOrElseToken extends Token

case object TrueToken extends TrueOrElseToken {
  override def tokenType: TokenType = TRUE

  override def value: String = "true"
}

case object FalseToken extends TrueOrElseToken {
  override def tokenType: TokenType = FALSE

  override def value: String = "false"
}

case object BreakToken extends Token {
  override def tokenType: TokenType = BREAK

  override def value: String = "break"
}

case object ReturnToken extends Token {
  override def tokenType: TokenType = RETURN

  override def value: String = "return"
}

case object FuncToken extends Token {
  override def tokenType: TokenType = FUNC

  override def value: String = "func"
}
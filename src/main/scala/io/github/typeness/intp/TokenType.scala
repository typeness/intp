package io.github.typeness.intp

sealed trait TokenType

// Literals

// eg. 456
case object INTEGER_CONST extends TokenType

// eg. 123.456
case object REAL_CONST extends TokenType

// eg. "3ab123"
case object STRING extends TokenType

// eg. '$'
case object CHARACTER extends TokenType

// Operators

// -
case object MINUS extends TokenType

// +
case object PLUS extends TokenType

// *
case object MULTIPLICATION extends TokenType

// /
case object DIV extends TokenType

// not
case object NOT extends TokenType

// or
case object OR extends TokenType

// and
case object AND extends TokenType

// =
case object ASSIGN extends TokenType

// >
case object GREATER extends TokenType

// >=
case object GREATER_OR_EQUALS extends TokenType

// <
case object LESS extends TokenType

// <=
case object LESS_OR_EQUALS extends TokenType

// ==
case object EQUALS extends TokenType

// !=
case object NOT_EQUALS extends TokenType

// %
case object MODULO extends TokenType

// -=
case object COMPOUND_MINUS extends TokenType

// +=
case object COMPOUND_PLUS extends TokenType

// *=
case object COMPOUND_MULTIPLICATION extends TokenType

// /=
case object COMPOUND_DIV extends TokenType

// %=
case object COMPOUND_MODULO extends TokenType

// Parenthesis

// (
case object L_ROUND_BRACKET extends TokenType

// )
case object R_ROUND_BRACKET extends TokenType

// [
case object L_SQUARE_BRACKET extends TokenType

// ]
case object R_SQUARE_BRACKET extends TokenType

// {
case object L_CURLY_BRACKET extends TokenType

// }
case object R_CURLY_BRACKET extends TokenType

// Misc

// \0
case object EOF extends TokenType

// eg. test
case object ID extends TokenType

// ,
case object COMMA extends TokenType

// '
case object APOSTROPHE extends TokenType

// .
case object DOT extends TokenType

// "
case object QUOTATION extends TokenType

// ;
case object SEMICOLON extends TokenType

// Keywords

// import
case object IMPORT extends TokenType

// while
case object WHILE extends TokenType

// if
case object IF extends TokenType

// else
case object ELSE extends TokenType

// true
case object TRUE extends TokenType

// false
case object FALSE extends TokenType

// break
case object BREAK extends TokenType

// return
case object RETURN extends TokenType

// func
case object FUNC extends TokenType

// then
case object THEN extends TokenType

// data
case object DATA extends TokenType

// for
case object FOR extends TokenType

// val
case object VAL extends TokenType

// var
case object VAR extends TokenType

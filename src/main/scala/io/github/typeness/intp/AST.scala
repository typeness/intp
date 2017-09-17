package io.github.typeness.intp


sealed trait AST {
  def token: Token
}

case class BinOp(left: AST, op: Token, right: AST) extends AST {
  override def token: Token = op
}

case class Number(token: Token) extends AST {
}

case class Boolean(token: Token) extends AST {
}

case class UnaryOp(op: Token, expr: AST) extends AST {
  override def token: Token = op
}

case class AssignAST(name: IdToken, expr: AST) extends AST {
  override def token: Token = AssignToken
}

case class ArrayAssignAST(name: IdToken, index: AST, expr: AST) extends AST {
  override def token: Token = name
}

case class VarAST(name: IdToken) extends AST {
  override def token: Token = name
}

case class Program(children: List[AST]) extends AST {
  override def token: Token = ???

  override def toString: String = s"Program(${children.map(_.toString).mkString("\n")})"
}

case class FunctionCall(name: IdToken, actualParameters: List[AST]) extends AST {
  override def token: Token = name
}

case class FunctionDefinition(formalParameters: List[IdToken],
                              body: Program) extends AST {
  override def token: Token = FuncToken
}

case class ArrayAccess(name: IdToken, index: AST) extends AST {
  override def token: Token = name
}

case class ArrayLiteral(elements: List[AST]) extends AST {
  override def token: Token = LSquareBracketToken
}

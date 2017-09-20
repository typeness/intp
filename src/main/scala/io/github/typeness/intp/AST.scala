package io.github.typeness.intp


sealed trait AST {
  def token: Token
}

case class BinOp(left: AST, op: Token, right: AST) extends AST {
  override def token: Token = op
}

case class Number(token: NumberToken) extends AST {
}

case class BooleanLiteral(token: TrueOrElseToken) extends AST {
}

case class CharLiteral(token: CharToken) extends AST {
}

case class UnaryOp(op: Token, expr: AST) extends AST {
  override def token: Token = op
}

case class AssignAST(name: IdToken, expr: AST) extends AST {
  override def token: Token = AssignToken
}

case class ArrayAssignAST(source: AST, index: AST, expr: AST) extends AST {
  override def token: Token = source.token
}

case class VarAST(name: IdToken) extends AST {
  override def token: Token = name
}

case class Program(children: List[AST]) extends AST {
  override def token: Token = ???

  override def toString: String = s"Program(${children.map(_.toString).mkString("\n")})"
}

case class FunctionCall(source: AST, actualParameters: List[AST]) extends AST {
  override def token: Token = source.token
}

case class FunctionLiteral(formalParameters: List[IdToken],
                           body: Program) extends AST {
  override def token: Token = FuncToken
}

case class ArrayAccess(source: AST, index: AST) extends AST {
  override def token: Token = source.token
}

case class ArrayLiteral(elements: List[AST]) extends AST {
  override def token: Token = LSquareBracketToken
}

case class IfAST(condition: AST, ifBlock: AST, elseBlock: Option[AST]) extends AST {
  override def token: IfToken.type = IfToken
}

case class WhileAST(condition: AST, whileBlock: AST) extends AST {
  override def token: WhileToken.type = WhileToken
}

case class ReturnAST(result: AST) extends AST {
  override def token: Token = ReturnToken
}
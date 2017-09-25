package io.github.typeness.intp


sealed trait AST {
  def token: Token
}

case class BinOp(left: AST, op: Token, right: AST) extends AST {
  override def token: Token = op
}

case class Number(token: NumberToken) extends AST

case class BooleanLiteral(token: TrueOrElseToken) extends AST

case class CharLiteral(token: CharToken) extends AST

case class UnaryOp(op: Token, expr: AST) extends AST {
  override def token: Token = op
}

case class AssignAST(name: IdToken, expr: AST, token: AssignToken) extends AST

case class ArrayAssignAST(source: AST, index: AST, expr: AST) extends AST {
  override def token: Token = source.token
}

case class VarAST(name: IdToken) extends AST {
  override def token: Token = name
}

case class Program(children: List[AST]) extends AST {
  override def token: Token = children.head.token

  override def toString: String = s"Program(List(${children.map(_.toString).mkString(",\n")}))"
}

case class FunctionCall(source: AST, actualParameters: List[AST]) extends AST {
  override def token: Token = source.token
}

case class FunctionLiteral(formalParameters: List[IdToken],
                           body: Program, token: FuncToken) extends AST

case class ArrayAccess(source: AST, index: AST) extends AST {
  override def token: Token = source.token
}

case class ArrayLiteral(elements: List[AST], token: LSquareBracketToken) extends AST

case class IfAST(condition: AST, ifBlock: AST, elseBlock: Option[AST], token: IfToken) extends AST

case class WhileAST(condition: AST, whileBlock: AST, token: WhileToken) extends AST

case class ReturnAST(result: AST, token: ReturnToken) extends AST
package io.github.typeness.intp

trait ASTVisitor {
  def visit(ast: AST): Any = ast match {
    case a: BinOp => binOp(a)
    case a: Number => number(a)
    case a: BooleanLiteral => booleanLiteral(a)
    case a: CharLiteral => charLiteral(a)
    case a: UnaryOp => unaryOp(a)
    case a: AssignAST => assignAST(a)
    case a: ArrayAssignAST => arrayAssignAST(a)
    case a: VarAST => varAST(a)
    case a: Program => program(a)
    case a: FunctionCall => functionCall(a)
    case a: FunctionDefinition => functionDefinition(a)
    case a: ArrayAccess => arrayAccess(a)
    case a: ArrayLiteral => arrayLiteral(a)
    case a: IfAST => ifAST(a)
    case a: WhileAST => whileAST(a)
    case a: ReturnAST => returnAST(a)
  }
  def binOp(ast: BinOp): Any
  def number(ast: Number): Any
  def booleanLiteral(ast: BooleanLiteral): Any
  def charLiteral(ast: CharLiteral): Any
  def unaryOp(ast: UnaryOp): Any
  def assignAST(ast: AssignAST): Any
  def arrayAssignAST(ast: ArrayAssignAST): Any
  def varAST(ast: VarAST): Any
  def program(ast: Program): Any
  def functionCall(ast: FunctionCall): Any
  def functionDefinition(ast: FunctionDefinition): Any
  def arrayAccess(ast: ArrayAccess): Any
  def arrayLiteral(ast: ArrayLiteral): Any
  def ifAST(ast: IfAST): Any
  def whileAST(ast: WhileAST): Any
  def returnAST(ast: ReturnAST): Any
}

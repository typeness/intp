package io.github.typeness.intp

trait ASTVisitor {
  def visit(ast: AST): Any = ast match {
    case a: BinOp               => binOp(a)
    case a: Number              => number(a)
    case a: BooleanLiteral      => booleanLiteral(a)
    case a: CharLiteral         => charLiteral(a)
    case a: UnaryOp             => unaryOp(a)
    case a: AssignAST           => assignAST(a)
    case a: ArrayAssignAST      => arrayAssignAST(a)
    case a: VarAST              => varAST(a)
    case a: Program             => program(a)
    case a: FunctionCall        => functionCall(a)
    case a: FunctionLiteral     => functionLiteral(a)
    case a: ArrayAccess         => arrayAccess(a)
    case a: ArrayLiteral        => arrayLiteral(a)
    case a: IfAST               => ifAST(a)
    case a: WhileAST            => whileAST(a)
    case a: ReturnAST           => returnAST(a)
    case a: BuiltinFunctionCall => builtinFunctionCall(a)
  }
  protected def binOp(ast: BinOp): Any
  protected def number(ast: Number): Any
  protected def booleanLiteral(ast: BooleanLiteral): Any
  protected def charLiteral(ast: CharLiteral): Any
  protected def unaryOp(ast: UnaryOp): Any
  protected def assignAST(ast: AssignAST): Any
  protected def arrayAssignAST(ast: ArrayAssignAST): Any
  protected def varAST(ast: VarAST): Any
  protected def program(ast: Program): Any
  protected def functionCall(ast: FunctionCall): Any
  protected def functionLiteral(ast: FunctionLiteral): Any
  protected def arrayAccess(ast: ArrayAccess): Any
  protected def arrayLiteral(ast: ArrayLiteral): Any
  protected def ifAST(ast: IfAST): Any
  protected def whileAST(ast: WhileAST): Any
  protected def returnAST(ast: ReturnAST): Any
  protected def builtinFunctionCall(ast: BuiltinFunctionCall): Any
}

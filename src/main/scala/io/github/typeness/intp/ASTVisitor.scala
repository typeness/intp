package io.github.typeness.intp

trait ASTVisitor {
  def visit(ast: AST): TopType = ast match {
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
    case a: ObjectLiteral       => objectLiteral(a)
    case a: PropertyAccess      => propertyAccess(a)
    case a: PropertyAssignAST   => propertyAssign(a)
    case a: IfThenElseAST       => ifThenElseAST(a)
    case a: ImportAST           => importAST(a)
  }
  protected def binOp(ast: BinOp): TopType
  protected def number(ast: Number): TopType
  protected def booleanLiteral(ast: BooleanLiteral): TopType
  protected def charLiteral(ast: CharLiteral): TopType
  protected def unaryOp(ast: UnaryOp): TopType
  protected def assignAST(ast: AssignAST): TopType
  protected def arrayAssignAST(ast: ArrayAssignAST): TopType
  protected def varAST(ast: VarAST): TopType
  protected def program(ast: Program): TopType
  protected def functionCall(ast: FunctionCall): TopType
  protected def functionLiteral(ast: FunctionLiteral): TopType
  protected def arrayAccess(ast: ArrayAccess): TopType
  protected def arrayLiteral(ast: ArrayLiteral): TopType
  protected def ifAST(ast: IfAST): TopType
  protected def whileAST(ast: WhileAST): TopType
  protected def returnAST(ast: ReturnAST): TopType
  protected def builtinFunctionCall(ast: BuiltinFunctionCall): TopType
  protected def objectLiteral(ast: ObjectLiteral): TopType
  protected def propertyAccess(ast: PropertyAccess): TopType
  protected def propertyAssign(ast: PropertyAssignAST): TopType
  protected def ifThenElseAST(ast: IfThenElseAST): TopType
  protected def importAST(ast: ImportAST): TopType
}

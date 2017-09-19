package io.github.typeness.intp

class Interpreter extends ASTVisitor {

  private def numericOperands[T](left: T, op: Token, right: T)(implicit num: Fractional[T]): T = {
    import num._
    op match {
      case AdditionToken => left + right
      case SubtractionToken => left - right
      case MultiplicationToken => left * right
      case DivisionToken => left / right
      case _ => throw InterpreterError(s"Undefined operator $left $op $right")
    }
  }

  override def binOp(ast: BinOp): Any = (visit(ast.left), visit(ast.right)) match {
    /* In case of double Ints to be able to call numericOperands method
        we need to cast one operand to Double then we can cast again to Int
      */
    case (left: Int, right: Int) => numericOperands(left.toDouble, ast.op, right).toInt
    case (left: Int, right: Double) => numericOperands(left, ast.op, right)
    case (left: Double, right: Int) => numericOperands(left, ast.op, right)
    case (left: Double, right: Double) => numericOperands(left, ast.op, right)
    case _ => ???
  }

  override def number(ast: Number): Any = ast.token match {
    case IntegerConstToken(intValue) => intValue
    case RealConstToken(doubleValue) => doubleValue
    case _ => throw InterpreterError(s"Not a number ${ast.token}")
  }

  override def booleanLiteral(ast: BooleanLiteral): Any = ???

  override def charLiteral(ast: CharLiteral): Any = ???

  override def unaryOp(ast: UnaryOp): Any = visit(ast.expr) match {
    case _: Double =>
    case value: Int => ast.op match {
      case AdditionToken => value
      case SubtractionToken => -value
      case _ => throw InterpreterError(s"Undefined unary operator ${ast.op}$value")
    }
    case _ => ???
  }

  override def assignAST(ast: AssignAST): Any = ???

  override def arrayAssignAST(ast: ArrayAssignAST): Any = ???

  override def varAST(ast: VarAST): Any = ???

  override def program(ast: Program): Any = ???

  override def functionCall(ast: FunctionCall): Any = ???

  override def functionDefinition(ast: FunctionDefinition): Any = ???

  override def arrayAccess(ast: ArrayAccess): Any = ???

  override def arrayLiteral(ast: ArrayLiteral): Any = ???

  override def ifAST(ast: IfAST): Any = ???

  override def whileAST(ast: WhileAST): Any = ???

  override def returnAST(ast: ReturnAST): Any = ???
}

case class InterpreterError(cause: String) extends Exception(cause)

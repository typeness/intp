package io.github.typeness.intp

import scala.collection.mutable

class Interpreter extends ASTVisitor {

  val globalScope: mutable.Map[String, Any] = mutable.Map.empty

  private def numericOperands[T](left: T, op: Token, right: T)(implicit num: Fractional[T]): T = {
    import num._
    op match {
      case AdditionToken => left + right
      case SubtractionToken => left - right
      case MultiplicationToken => left * right
      case DivisionToken => left / right
      case _ => throw UndefinedBinaryOp(left, op, right)
    }
  }

  private def booleanOperands(left: Boolean, op: Token, right: Boolean): Any = op match {
    case AndToken => left && right
    case OrToken => left || right
    case _ => throw UndefinedBinaryOp(left, op, right)
  }

  override protected def binOp(ast: BinOp): Any = (visit(ast.left), visit(ast.right)) match {
    /* In case of double Ints to be able to call numericOperands method
        we need to cast one operand to Double then we can cast again to Int
      */
    case (left: Int, right: Int) => numericOperands(left.toDouble, ast.op, right).toInt
    case (left: Int, right: Double) => numericOperands(left, ast.op, right)
    case (left: Double, right: Int) => numericOperands(left, ast.op, right)
    case (left: Double, right: Double) => numericOperands(left, ast.op, right)
    case (left: Boolean, right: Boolean) => booleanOperands(left, ast.op, right)
    case (left, right) => throw UndefinedBinaryOp(left, ast.op, right)
  }

  override protected def number(ast: Number): Any = ast.token match {
    case IntegerConstToken(intValue) => intValue
    case RealConstToken(doubleValue) => doubleValue
  }

  override protected def booleanLiteral(ast: BooleanLiteral): Any = ast match {
    case BooleanLiteral(TrueToken) => true
    case BooleanLiteral(FalseToken) => false
  }

  override protected def charLiteral(ast: CharLiteral): Any = ???

  override protected def unaryOp(ast: UnaryOp): Any = visit(ast.expr) match {
    case value: Double => ast.op match {
      case AdditionToken => value
      case SubtractionToken => -value
      case _ => throw UndefinedUnaryOp(ast.op, value)
    }
    case value: Int => ast.op match {
      case AdditionToken => value
      case SubtractionToken => -value
      case _ => throw UndefinedUnaryOp(ast.op, value)
    }
    case value: Boolean if ast.op == NotToken => !value
    case value => throw UndefinedUnaryOp(ast.op, value)
  }

  override protected def assignAST(ast: AssignAST): Any = {
    val name = ast.name.value
    globalScope.put(name, visit(ast.expr))
    ()
  }

  override protected def arrayAssignAST(ast: ArrayAssignAST): Any = ???

  override protected def varAST(ast: VarAST): Any = globalScope.get(ast.name.value) match {
    case Some(variable) => variable
    case None => throw new InterpreterError(s"Variable not found ${ast.name.value}") {}
  }

  override protected def program(ast: Program): Any = ???

  override protected def functionCall(ast: FunctionCall): Any = ???

  override protected def functionDefinition(ast: FunctionDefinition): Any = ???

  override protected def arrayAccess(ast: ArrayAccess): Any = ???

  override protected def arrayLiteral(ast: ArrayLiteral): Any = ???

  override protected def ifAST(ast: IfAST): Any = ???

  override protected def whileAST(ast: WhileAST): Any = ???

  override protected def returnAST(ast: ReturnAST): Any = ???
}

abstract class InterpreterError(cause: String) extends Exception(cause)

case class UndefinedUnaryOp(op: Token, value: Any) extends InterpreterError(s"Undefined unary operator $op$value")

case class UndefinedBinaryOp(left: Any, op: Token, right: Any) extends InterpreterError(s"Undefined operator $left $op $right")


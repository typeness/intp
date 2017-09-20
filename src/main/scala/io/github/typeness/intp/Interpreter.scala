package io.github.typeness.intp

import scala.annotation.tailrec

class Interpreter extends ASTVisitor {

  val memory: Memory = new Memory()

  private def numericOperands[T](left: T, op: Token, right: T)(implicit num: Fractional[T]): Any = {
    import num._
    op match {
      case AdditionToken => left + right
      case SubtractionToken => left - right
      case MultiplicationToken => left * right
      case DivisionToken => left / right
      case EqualsToken => left == right
      case GreaterOrEqualsToken => left >= right
      case LessOrEqualsToken => left <= right
      case GreaterToken => left > right
      case LessToken => left < right
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
    case (left: Int, right: Int) => numericOperands(left.toDouble, ast.op, right) match {
      case value: Double => value.toInt
      case value => value
    }
    case (left: Int, right: Double) => numericOperands(left, ast.op, right)
    case (left: Double, right: Int) => numericOperands(left, ast.op, right)
    case (left: Double, right: Double) => numericOperands(left, ast.op, right)
    case (left: Boolean, right: Boolean) => booleanOperands(left, ast.op, right)
    case (left: Vector[_], right: Vector[_]) => left ++ right
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

  override protected def charLiteral(ast: CharLiteral): Any = ast.token.c

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
    memory.define(name -> visit(ast.expr))
    ()
  }

  override protected def arrayAssignAST(ast: ArrayAssignAST): Any = ???

  override protected def varAST(ast: VarAST): Any = memory.get(ast.name.value) match {
    case Some(variable) => variable
    case None => throw new InterpreterError(s"Variable not found ${ast.name.value}") {}
  }

  override protected def program(ast: Program): Any = ast.children.foreach(visit)

  override protected def functionCall(ast: FunctionCall): Any = ???

  override protected def functionDefinition(ast: FunctionDefinition): Any = ???

  override protected def arrayAccess(ast: ArrayAccess): Any = visit(ast.source) match {
    case ls: Vector[_] => visit(ast.index) match {
      case i: Int => ls(i)
      case value => throw TypeMismatch(s"excepted integer expression as array index. Not $value")
    }
    case value => throw new InterpreterError(s"Not an array $value") {}
  }

  override protected def arrayLiteral(ast: ArrayLiteral): Any = ast.elements.map(visit).toVector

  override protected def ifAST(ast: IfAST): Any = visit(ast.condition) match {
    case true => visit(ast.ifBlock)
    case false => ast.elseBlock.foreach(visit)
    case value => throw TypeMismatch(s"excepted boolean expression in if statement not $value")
  }

  @tailrec
  final override protected def whileAST(ast: WhileAST): Any = visit(ast.condition) match {
    case true =>
      visit(ast.whileBlock)
      whileAST(ast)
    case false => ()
    case value => throw TypeMismatch(s"excepted boolean expression in if statement not $value")
  }

  override protected def returnAST(ast: ReturnAST): Any = ???
}

abstract class InterpreterError(cause: String) extends Exception(cause)

case class UndefinedUnaryOp(op: Token, value: Any) extends InterpreterError(s"Undefined unary operator $op$value")

case class UndefinedBinaryOp(left: Any, op: Token, right: Any) extends InterpreterError(s"Undefined operator $left $op $right")

case class TypeMismatch(cause: String) extends InterpreterError(s"Type mismatch $cause")


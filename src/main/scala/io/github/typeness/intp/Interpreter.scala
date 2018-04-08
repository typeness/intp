package io.github.typeness.intp

import scala.collection.mutable

class Interpreter extends ASTVisitor {

  val memory: Memory = new Memory()
  private var fileName: String = "<console>"
  private var compilationUnit: CompilationUnit = CompilationUnit(fileName, "")

  private def numericOperands[T](left: T, op: Token, right: T)(implicit num: Fractional[T]): Any = {
    import num._
    op.tokenType match {
      case PLUS => left + right
      case MINUS => left - right
      case MULTIPLICATION => left * right
      case DIV => left / right
      case EQUALS => left == right
      case GREATER_OR_EQUALS => left >= right
      case LESS_OR_EQUALS => left <= right
      case GREATER => left > right
      case LESS => left < right
      case NOT_EQUALS => left != right
      case L_ROUND_BRACKET => ()
      // Fractional typeclass have no defined modulo operator
      case MODULO => left.toDouble() % right.toDouble()
      case _ =>
        throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }
  }

  private def booleanOperands(left: Boolean, op: Token, right: Boolean): Any =
    op.tokenType match {
      case AND => left && right
      case OR => left || right
      case EQUALS => left == right
      case NOT_EQUALS => left != right
      case _ =>
        throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }

  private def charOperands(left: Char, op: Token, right: Char): Any =
    op.tokenType match {
      case EQUALS => left == right
      case NOT_EQUALS => left != right
      case _ =>
        throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }

  private def arrayOperands(left: mutable.ArrayBuffer[_],
                            op: Token,
                            right: mutable.ArrayBuffer[_]): Any = op.tokenType match {
    case PLUS => left ++ right
    case EQUALS => left == right
    case NOT_EQUALS => left != right
    case _ =>
      throw WrongBinaryOperator(
        s"[${left.mkString(", ")}]",
        op,
        s"[${right.mkString(", ")}]",
        compilationUnit,
        op.position
      )
  }

  override protected def binOp(ast: BinOp): Any =
    (visit(ast.left), visit(ast.right)) match {
      /* In case of double Ints to be able to call numericOperands method
        we need to cast one operand to Double then we can cast again to Int
       */
      case (left: Int, right: Int) =>
        numericOperands(left.toDouble, ast.op, right) match {
          case value: Double => value.toInt
          case value => value
        }
      case (left: Int, right: Double) => numericOperands(left, ast.op, right)
      case (left: Double, right: Int) => numericOperands(left, ast.op, right)
      case (left: Double, right: Double) => numericOperands(left, ast.op, right)
      case (left: Boolean, right: Boolean) =>
        booleanOperands(left, ast.op, right)
      case (left: mutable.ArrayBuffer[_], right: mutable.ArrayBuffer[_]) =>
        arrayOperands(left, ast.op, right)
      case (left: Char, right: Char) => charOperands(left, ast.op, right)
      case (left, right) =>
        throw WrongBinaryOperator(left, ast.op, right, compilationUnit, ast.op.position)
    }

  override protected def number(ast: Number): Any = ast.token match {
    case IntegerConstToken(intValue, _) => intValue
    case RealConstToken(doubleValue, _) => doubleValue
  }

  override protected def booleanLiteral(ast: BooleanLiteral): Any = ast match {
    case BooleanLiteral(TrueToken(_)) => true
    case BooleanLiteral(FalseToken(_)) => false
  }

  override protected def charLiteral(ast: CharLiteral): Any = ast.token.c

  override protected def unaryOp(ast: UnaryOp): Any = visit(ast.expr) match {
    case value: Double =>
      ast.op.tokenType match {
        case PLUS => value
        case MINUS => -value
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            ast.expr.token.value,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case value: Int =>
      ast.op.tokenType match {
        case PLUS => value
        case MINUS => -value
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            ast.expr.token.value,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case value: Boolean if ast.op.tokenType == NOT => !value
    case _ =>
      throw WrongUnaryOperator(
        ast.op,
        ast.expr.token.value,
        compilationUnit,
        ast.expr.token.position
      )
  }

  override protected def assignAST(ast: AssignAST): Any = {
    val name = ast.name.value
    ast.expr match {
      // do not evaluate function definition
      case fl: FunctionLiteral => memory.define(name -> fl)
      case _ => memory.define(name -> visit(ast.expr))
    }
    ()
  }

  override protected def arrayAssignAST(ast: ArrayAssignAST): Any =
    visit(ast.source) match {
      case arr: mutable.ArrayBuffer[Any@unchecked] =>
        visit(ast.index) match {
          case index: Int =>
            arr(index) = visit(ast.expr)
          case value =>
            throw TypeMismatch(value, IntegerType, compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, ArrayType, compilationUnit, ast.source.token.position)
    }

  override protected def varAST(ast: VarAST): Any =
    memory.get(ast.name.value) match {
      case Some(variable) => variable
      case None =>
        throw UndefinedVariable(ast.name.value, compilationUnit, ast.name.position)
    }

  override protected def program(ast: Program): Any = {
    for (child <- ast.children if memory.get("return").isEmpty) visit(child)
  }

  override protected def functionCall(ast: FunctionCall): Any =
    visit(ast.source) match {
      case FunctionLiteral(formalParameters, body, _) =>
        if (formalParameters.size != ast.actualParameters.size) {
          val fnName = ast.source match {
            case VarAST(name) => name.value
            case _ => "<anonymous>"
          }
          throw WrongFunctionCall(
            fnName,
            ast.actualParameters.size,
            formalParameters.size,
            compilationUnit,
            ast.source.token.position
          )
        }
        val parameters = formalParameters
          .zip(ast.actualParameters)
          .map({
            case (idToken, expr) => (idToken.value, visit(expr))
          })
        memory.pushNewStack()
        parameters.foreach(memory.define)
        visit(body)
        val result = memory.get("return").orElse(Some(())).get
        memory.popStack()
        result
      case value =>
        throw TypeMismatch(value, FunctionType, compilationUnit, ast.source.token.position)
    }

  override protected def functionLiteral(ast: FunctionLiteral): Any = ast

  override protected def arrayAccess(ast: ArrayAccess): Any =
    visit(ast.source) match {
      case ls: mutable.ArrayBuffer[_] =>
        visit(ast.index) match {
          case i: Int => ls(i)
          case value =>
            throw TypeMismatch(value, IntegerType, compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, ArrayType, compilationUnit, ast.source.token.position)
    }

  override protected def arrayLiteral(ast: ArrayLiteral): Any =
    ast.elements.map(visit).to[mutable.ArrayBuffer]

  override protected def ifAST(ast: IfAST): Any = visit(ast.condition) match {
    case true =>
      memory.pushNewLocalScope()
      visit(ast.ifBlock)
      memory.popNewLocalScope()
    case false =>
      ast.elseBlock match {
        case Some(p) =>
          memory.pushNewLocalScope()
          visit(p)
          memory.popNewLocalScope()
        case None => ()
      }
    case value =>
      throw TypeMismatch(value, BooleanType, compilationUnit, ast.condition.token.position)
  }

  final override protected def whileAST(ast: WhileAST): Any = {
    if (memory.get("return").nonEmpty) ()
    var condition = visit(ast.condition)
    memory.pushNewLocalScope()
    while (condition != false && memory.get("return").isEmpty) {
      condition match {
        case true =>
          visit(ast.whileBlock)
        case value => throw TypeMismatch(value, BooleanType, compilationUnit, ast.whileBlock.token.position)
      }
      if (memory.get("return").nonEmpty) condition = false
      else condition = visit(ast.condition)
    }
    memory.popNewLocalScope()
  }

  override protected def returnAST(ast: ReturnAST): Any =
    visit(
      AssignAST(IdToken("return", ast.token.position), ast.result, AssignToken(ast.token.position))
    )

  override protected def builtinFunctionCall(ast: BuiltinFunctionCall): Any = {
    val name = ast.name.value
    BuiltinFunctions.map.get(name) match {
      case Some(fn) =>
        val params = ast.actualParameters.map(visit)
        fn(if (params.size == 1) params.head else params, compilationUnit, ast.token.position)
      case None => throw UndefinedVariable(name, compilationUnit, ast.token.position)
    }
  }

  override protected def objectLiteral(ast: ObjectLiteral): mutable.Map[String, Any] =
    mutable.Map(ast.elements.map { case (name, value) => (name.value, visit(value)) }.toSeq: _*)

  override protected def propertyAccess(ast: PropertyAccess): Any = visit(ast.source) match {
    case objectLiteral: mutable.Map[String@unchecked, _] =>
      objectLiteral.get(ast.name.value) match {
        case Some(x) => x
        case None => throw UndefinedVariable(ast.name.value, compilationUnit, ast.token.position)
      }
    case value =>
      throw TypeMismatch(value, ObjectType, compilationUnit, ast.source.token.position)
  }

  override protected def propertyAssign(ast: PropertyAssignAST): Any =
    visit(ast.source) match {
      case map: mutable.Map[String@unchecked, Any@unchecked] =>
        map(ast.name.value) = visit(ast.expr)
      case value => throw TypeMismatch(value, ObjectType, compilationUnit, ast.source.token.position)

    }

  def runFromResource(res: String): Any = {
    fileName = res
    val parser = Parser.fromResource(res)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

  def runFromString(source: String): Any = {
    fileName = source
    val parser = new Parser(source)(compilationUnit)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

  def runFromFile(filename: String): Any = {
    this.fileName = filename
    val parser = Parser.fromFile(filename)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

}

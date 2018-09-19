package io.github.typeness.intp

import scala.collection.mutable

class Interpreter extends ASTVisitor {

  val memory: Memory = new Memory()
  private var fileName: String = "<console>"
  private var compilationUnit: CompilationUnit = CompilationUnit(fileName, "")

  private def integerOperands(left: Int, op: Token, right: Int): TopType = op.tokenType match {
    case PLUS => IntegerType(left + right)
    case MINUS => IntegerType(left - right)
    case MULTIPLICATION => IntegerType(left * right)
    case DIV => IntegerType(left / right)
    case EQUALS => BooleanType(left == right)
    case GREATER_OR_EQUALS => BooleanType(left >= right)
    case LESS_OR_EQUALS => BooleanType(left <= right)
    case GREATER => BooleanType(left > right)
    case LESS => BooleanType(left < right)
    case NOT_EQUALS => BooleanType(left != right)
    case L_ROUND_BRACKET => UnitType
    case MODULO => DoubleType(left % right)
    case _ =>
      throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
  }

  private def doubleOperands(left: Double, op: Token, right: Double): TopType = op.tokenType match {
    case PLUS => DoubleType(left + right)
    case MINUS => DoubleType(left - right)
    case MULTIPLICATION => DoubleType(left * right)
    case DIV => DoubleType(left / right)
    case EQUALS => BooleanType(left == right)
    case GREATER_OR_EQUALS => BooleanType(left >= right)
    case LESS_OR_EQUALS => BooleanType(left <= right)
    case GREATER => BooleanType(left > right)
    case LESS => BooleanType(left < right)
    case NOT_EQUALS => BooleanType(left != right)
    case L_ROUND_BRACKET => UnitType
    case MODULO => DoubleType(left % right)
    case _ =>
      throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
  }

  private def booleanOperands(left: Boolean, op: Token, right: Boolean): BooleanType =
    op.tokenType match {
      case AND => BooleanType(left && right)
      case OR => BooleanType(left || right)
      case EQUALS => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ =>
        throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }

  private def charOperands(left: Char, op: Token, right: Char): TopType =
    op.tokenType match {
      case EQUALS => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ =>
        throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }

  private def arrayOperands(left: mutable.ArrayBuffer[TopType@unchecked],
                            op: Token,
                            right: mutable.ArrayBuffer[TopType@unchecked]): TopType =
    op.tokenType match {
      case PLUS => ArrayType(left ++ right)
      case EQUALS => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ =>
        throw WrongBinaryOperator(
          s"[${left.mkString(", ")}]",
          op,
          s"[${right.mkString(", ")}]",
          compilationUnit,
          op.position
        )
    }

  private def objectOperands(left: ObjectType, op: Token, right: ObjectType): TopType =
    op.tokenType match {
      case EQUALS => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ => throw WrongBinaryOperator(left, op, right, compilationUnit, op.position)
    }

  override protected def binOp(ast: BinOp): TopType =
    (visit(ast.left), visit(ast.right)) match {
      /* In case of double Ints to be able to call numericOperands method
        we need to cast one operand to Double then we can cast again to Int
       */
      case (IntegerType(left), IntegerType(right)) =>
        //      case (left: Int, right: Int) =>
        integerOperands(left, ast.op, right)
      case (IntegerType(left), DoubleType(right)) => doubleOperands(left.toDouble, ast.op, right)
      case (DoubleType(left), IntegerType(right)) => doubleOperands(left, ast.op, right.toDouble)
      case (DoubleType(left), DoubleType(right)) => doubleOperands(left, ast.op, right)
      case (BooleanType(left), BooleanType(right)) =>
        booleanOperands(left, ast.op, right)
      case (ArrayType(left), ArrayType(right)) =>
        arrayOperands(left, ast.op, right)
      case (CharType(left), CharType(right)) => charOperands(left, ast.op, right)
      case (left@ObjectType(_), right@ObjectType(_)) => objectOperands(left, ast.op, right)
      case (left, right) =>
        throw WrongBinaryOperator(left, ast.op, right, compilationUnit, ast.op.position)
    }

  override protected def number(ast: Number): TopType = ast.token match {
    case IntegerConstToken(intValue, _) => IntegerType(intValue)
    case RealConstToken(doubleValue, _) => DoubleType(doubleValue)
  }

  override protected def booleanLiteral(ast: BooleanLiteral): BooleanType = ast match {
    case BooleanLiteral(TrueToken(_)) => BooleanType(true)
    case BooleanLiteral(FalseToken(_)) => BooleanType(false)
  }

  override protected def charLiteral(ast: CharLiteral): CharType = CharType(ast.token.c)

  override protected def unaryOp(ast: UnaryOp): TopType = visit(ast.expr) match {
    case DoubleType(value) =>
      ast.op.tokenType match {
        case PLUS => DoubleType(value)
        case MINUS => DoubleType(-value)
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            ast.expr.token.value,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case IntegerType(value) =>
      ast.op.tokenType match {
        case PLUS => IntegerType(value)
        case MINUS => IntegerType(-value)
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            ast.expr.token.value,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case BooleanType(value) if ast.op.tokenType == NOT => BooleanType(!value)
    case _ =>
      throw WrongUnaryOperator(
        ast.op,
        ast.expr.token.value,
        compilationUnit,
        ast.expr.token.position
      )
  }

  override protected def assignAST(ast: AssignAST): TopType = {
    val name = ast.name.value
    ast.expr match {
      // do not evaluate function definition
      case fl: FunctionLiteral =>
        memory.define(name -> FunctionType(fl, memory.getAll.toList))
      case _ => memory.define(name -> visit(ast.expr))
    }
    UnitType
  }

  override protected def arrayAssignAST(ast: ArrayAssignAST): TopType =
    visit(ast.source) match {
      case ArrayType(arr: mutable.ArrayBuffer[TopType@unchecked]) =>
        visit(ast.index) match {
          case IntegerType(index) =>
            arr(index) = visit(ast.expr)
            UnitType
          case value =>
            throw TypeMismatch(value, "Integer", compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, "Array", compilationUnit, ast.source.token.position)
    }

  override protected def varAST(ast: VarAST): TopType =
    memory.get(ast.name.value) match {
      case Some(variable) => variable.value
      case None =>
        throw UndefinedVariable(ast.name.value, compilationUnit, ast.name.position)
    }

  override protected def program(ast: Program): TopType = {
    for (child <- ast.children if memory.get("return").isEmpty) visit(child)
    UnitType
  }

  override protected def functionCall(ast: FunctionCall): TopType =
    visit(ast.source) match {
      case FunctionType(FunctionLiteral(formalParameters, body, _), closures) =>
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
        } else {
          val parameters = formalParameters
            .zip(ast.actualParameters)
            .map({
              case (idToken, expr) => (idToken.value, visit(expr))
            })
          memory.pushNewStack()
          closures.foreach { case (name, obj) =>
            memory.get(name) match {
              case Some(oldObj) =>
                if (oldObj.scopeLevel < obj.scopeLevel) memory.define(name -> obj.value)
                else ()
              case None => memory.define(name -> obj.value)
            }
          }
          parameters.foreach(memory.define)
          visit(body)
          val result = memory.get("return") match {
            case Some(ObjectInMemory(_, value, _)) => value
            case None => UnitType
          }
          memory.popStack()
          result
        }
      case value =>
        throw TypeMismatch(value, "Function", compilationUnit, ast.source.token.position)
    }

  override protected def functionLiteral(ast: FunctionLiteral): TopType =
    FunctionType(ast, memory.getAll.toList)

  override protected def arrayAccess(ast: ArrayAccess): TopType =
    visit(ast.source) match {
      case ArrayType(ls: mutable.ArrayBuffer[TopType@unchecked]) =>
        visit(ast.index) match {
          case IntegerType(i) =>
            if (i >= 0 && i < ls.size) ls(i)
            else throw IndexOutOfBound(ast.token.value, ls.size, i, compilationUnit, ast.token.position)
          case value =>
            throw TypeMismatch(value, "Integer", compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, "Array", compilationUnit, ast.source.token.position)
    }

  override protected def arrayLiteral(ast: ArrayLiteral): TopType =
    ArrayType(ast.elements.map(visit).to[mutable.ArrayBuffer])

  override protected def ifAST(ast: IfAST): TopType = visit(ast.condition) match {
    case BooleanType(true) =>
      memory.pushNewLocalScope()
      visit(ast.ifBlock)
      memory.popNewLocalScope()
      UnitType
    case BooleanType(false) =>
      ast.elseBlock match {
        case Some(p) =>
          memory.pushNewLocalScope()
          visit(p)
          memory.popNewLocalScope()
          UnitType
        case None => UnitType
      }
    case value =>
      throw TypeMismatch(value, "Boolean", compilationUnit, ast.condition.token.position)
  }

  final override protected def whileAST(ast: WhileAST): TopType = {
    if (memory.get("return").nonEmpty) ()
    var BooleanType(condition) = visit(ast.condition) match {
      case b: BooleanType => b
      case value =>
        throw TypeMismatch(value, "Boolean", compilationUnit, ast.whileBlock.token.position)
    }
    memory.pushNewLocalScope()
    while (condition && memory.get("return").isEmpty) {
      condition match {
        case true =>
          visit(ast.whileBlock)
        case value =>
          throw TypeMismatch(value, "Boolean", compilationUnit, ast.whileBlock.token.position)
      }
      if (memory.get("return").nonEmpty) condition = false
      else {
        val BooleanType(updated) = visit(ast.condition)
        condition = updated
      }
    }
    memory.popNewLocalScope()
    UnitType
  }

  override protected def returnAST(ast: ReturnAST): TopType =
    visit(
      AssignAST(IdToken("return", ast.token.position), ast.result, AssignToken(ast.token.position))
    )

  override protected def builtinFunctionCall(ast: BuiltinFunctionCall): TopType = {
    val name = ast.name.value
    Builtin.functions.get(name) match {
      case Some(fn) =>
        val params = ast.actualParameters.map(visit)
        fn(params, compilationUnit, ast.token.position)
      case None => throw UndefinedVariable(name, compilationUnit, ast.token.position)
    }
  }

  override protected def objectLiteral(ast: ObjectLiteral): TopType =
    ObjectType(
      mutable.Map(ast.elements.map { case (name, value) => (name.value, visit(value)) }.toSeq: _*)
    )

  override protected def propertyAccess(ast: PropertyAccess): TopType = visit(ast.source) match {
    case ObjectType(objectLiteral: mutable.Map[String@unchecked, TopType@unchecked]) =>
      objectLiteral.get(ast.name.value) match {
        case Some(x) => x
        case None => throw UndefinedVariable(ast.name.value, compilationUnit, ast.token.position)
      }
    case value =>
      throw TypeMismatch(value, "Object", compilationUnit, ast.source.token.position)
  }

  override protected def propertyAssign(ast: PropertyAssignAST): TopType =
    visit(ast.source) match {
      case ObjectType(map: mutable.Map[String@unchecked, TopType@unchecked]) =>
        map(ast.name.value) = visit(ast.expr)
        UnitType
      case value => throw TypeMismatch(value, "Object", compilationUnit, ast.source.token.position)

    }

  override protected def ifThenElseAST(ast: IfThenElseAST): TopType = visit(ast.condition) match {
    case BooleanType(true) => visit(ast.ifBlock)
    case BooleanType(false) => visit(ast.elseBlock)
    case value =>
      throw TypeMismatch(value, "Boolean", compilationUnit, ast.condition.token.position)
  }

  override protected def importAST(ast: ImportAST): TopType = {
    val interpreter = new Interpreter()
    interpreter.runFromFile(ast.token.value + ".intp")
    // copy definitions after running interpreter
    interpreter.memory.getAll.foreach{
      case (name, obj) => memory.define(name -> obj.value)
    }
    UnitType
  }

  override protected def forAST(ast: ForAST): TopType = {
    if (memory.get("return").nonEmpty) ()
    memory.pushNewLocalScope()
    ast.initial.foreach(visit)
    var BooleanType(condition) = visit(ast.condition) match {
      case b: BooleanType => b
      case value =>
        throw TypeMismatch(value, "Boolean", compilationUnit, ast.program.token.position)
    }
    while (condition && memory.get("return").isEmpty) {
      condition match {
        case true =>
          visit(ast.program)
        case value =>
          throw TypeMismatch(value, "Boolean", compilationUnit, ast.program.token.position)
      }
      if (memory.get("return").nonEmpty) condition = false
      else {
        ast.step.foreach(visit)
        val BooleanType(updated) = visit(ast.condition)
        condition = updated
      }
    }
    memory.popNewLocalScope()
    UnitType
  }

  def runFromResource(res: String): TopType = {
    fileName = res
    val parser = Parser.fromResource(res)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

  def runFromString(source: String): TopType = {
    fileName = source
    val parser = new Parser(source)(compilationUnit)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

  def runFromFile(filename: String): TopType = {
    this.fileName = filename
    val parser = Parser.fromFile(filename)
    compilationUnit = parser.compilationUnit
    val ast = parser.parse()
    visit(ast)
  }

}

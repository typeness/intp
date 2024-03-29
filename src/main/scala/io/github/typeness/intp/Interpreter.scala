package io.github.typeness.intp

import scala.collection.mutable

class Interpreter extends ASTVisitor {

  val memory: Memory = new Memory()
  private var fileName: String = "<console>"
  private var compilationUnit: CompilationUnit = CompilationUnit(fileName, "")

  private def integerOperands(left: Int, op: Token, right: Int): TopType = op.tokenType match {
    case PLUS              => IntegerType(left + right)
    case MINUS             => IntegerType(left - right)
    case MULTIPLICATION    => IntegerType(left * right)
    case DIV               => IntegerType(left / right)
    case EQUALS            => BooleanType(left == right)
    case GREATER_OR_EQUALS => BooleanType(left >= right)
    case LESS_OR_EQUALS    => BooleanType(left <= right)
    case GREATER           => BooleanType(left > right)
    case LESS              => BooleanType(left < right)
    case NOT_EQUALS        => BooleanType(left != right)
    case L_ROUND_BRACKET   => UnitType
    case MODULO            => DoubleType(left % right)
    case _ =>
      throw WrongBinaryOperator(Type.Integer, op, Type.Integer, compilationUnit, op.position)
  }

  private def doubleOperands(left: Double, op: Token, right: Double): TopType = op.tokenType match {
    case PLUS              => DoubleType(left + right)
    case MINUS             => DoubleType(left - right)
    case MULTIPLICATION    => DoubleType(left * right)
    case DIV               => DoubleType(left / right)
    case EQUALS            => BooleanType(left == right)
    case GREATER_OR_EQUALS => BooleanType(left >= right)
    case LESS_OR_EQUALS    => BooleanType(left <= right)
    case GREATER           => BooleanType(left > right)
    case LESS              => BooleanType(left < right)
    case NOT_EQUALS        => BooleanType(left != right)
    case L_ROUND_BRACKET   => UnitType
    case MODULO            => DoubleType(left % right)
    case _ =>
      throw WrongBinaryOperator(Type.Double, op, Type.Double, compilationUnit, op.position)
  }

  private def booleanOperands(left: Boolean, op: Token, right: AST): BooleanType = {
    def evalRight(right: AST): BooleanType = visit(right) match {
      case b: BooleanType => b
      case _ => throw WrongBinaryOperator(Type.Boolean, op, Type.Boolean, compilationUnit, op.position)
    }
    (left, op.tokenType) match {
      case (false, AND)       => BooleanType(false)
      case (true, AND)        => evalRight(right)
      case (true, OR)         => BooleanType(true)
      case (false, OR)        => evalRight(right)
      case (_, EQUALS)        => BooleanType(left == evalRight(right).value)
      case (_, NOT_EQUALS)    => BooleanType(left != evalRight(right).value)
      case _ =>
        throw WrongBinaryOperator(Type.Boolean, op, Type.Boolean, compilationUnit, op.position)
    }

  }
  private def charOperands(left: Char, op: Token, right: Char): TopType =
    op.tokenType match {
      case EQUALS     => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ =>
        throw WrongBinaryOperator(Type.Char, op, Type.Char, compilationUnit, op.position)
    }

  private def arrayOperands(left: mutable.ArrayBuffer[TopType @unchecked],
                            op: Token,
                            right: mutable.ArrayBuffer[TopType @unchecked]): TopType =
    op.tokenType match {
      case PLUS       => ArrayType(left ++ right)
      case EQUALS     => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _ =>
        throw WrongBinaryOperator(
          Type.Array,
          op,
          Type.Array,
          compilationUnit,
          op.position
        )
    }

  private def objectOperands(left: ObjectType, op: Token, right: ObjectType): TopType =
    op.tokenType match {
      case EQUALS     => BooleanType(left == right)
      case NOT_EQUALS => BooleanType(left != right)
      case _          =>
        throw WrongBinaryOperator(Type.Object, op, Type.Object, compilationUnit, op.position)
    }

  override protected def binOp(ast: BinOp): TopType = {
    val leftOperand = visit(ast.left)
    leftOperand match {
      // second operand is lazy for short circuit evaluation
      case BooleanType(value) => booleanOperands(value, ast.token, ast.right)
      case _ => (leftOperand, visit(ast.right)) match {
        /* In case of double Ints to be able to call numericOperands method
          we need to cast one operand to Double then we can cast again to Int
         */
        case (IntegerType(left), IntegerType(right)) =>
          //      case (left: Int, right: Int) =>
          integerOperands(left, ast.op, right)
        case (IntegerType(left), DoubleType(right)) => doubleOperands(left.toDouble, ast.op, right)
        case (DoubleType(left), IntegerType(right)) => doubleOperands(left, ast.op, right.toDouble)
        case (DoubleType(left), DoubleType(right))  => doubleOperands(left, ast.op, right)
        case (ArrayType(left), ArrayType(right)) =>
          arrayOperands(left, ast.op, right)
        case (CharType(left), CharType(right))             => charOperands(left, ast.op, right)
        case (left @ ObjectType(_), right @ ObjectType(_)) => objectOperands(left, ast.op, right)
        case (left, right) =>
          throw WrongBinaryOperator(
            Type.toEnum(left), ast.op, Type.toEnum(right), compilationUnit, ast.op.position
          )
      }
    }
  }

  override protected def number(ast: Number): TopType = ast.token match {
    case IntegerConstToken(intValue, _) => IntegerType(intValue)
    case RealConstToken(doubleValue, _) => DoubleType(doubleValue)
  }

  override protected def booleanLiteral(ast: BooleanLiteral): BooleanType = ast match {
    case BooleanLiteral(TrueToken(_))  => BooleanType(true)
    case BooleanLiteral(FalseToken(_)) => BooleanType(false)
  }

  override protected def charLiteral(ast: CharLiteral): CharType = CharType(ast.token.c)

  override protected def unaryOp(ast: UnaryOp): TopType = visit(ast.expr) match {
    case DoubleType(value) =>
      ast.op.tokenType match {
        case PLUS  => DoubleType(value)
        case MINUS => DoubleType(-value)
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            Type.Double,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case IntegerType(value) =>
      ast.op.tokenType match {
        case PLUS  => IntegerType(value)
        case MINUS => IntegerType(-value)
        case _ =>
          throw WrongUnaryOperator(
            ast.op,
            Type.Integer,
            compilationUnit,
            ast.expr.token.position
          )
      }
    case BooleanType(value) if ast.op.tokenType == NOT => BooleanType(!value)
    case _ =>
      throw WrongUnaryOperator(
        ast.op,
        Type.Boolean,
        compilationUnit,
        ast.expr.token.position
      )
  }

  override protected def assignAST(ast: AssignAST): TopType = {
    val name = ast.name.value
    memory.get(name) match {
      case Some(objectInMemory) =>
        if (objectInMemory.isMutable) {
          ast.expr match {
            // do not evaluate function definition
            case fl: FunctionLiteral =>
              memory.assign(name -> FunctionType(fl, memory.getAll.toList))
            case _ => memory.assign(name -> visit(ast.expr))
          }
        } else {
          throw ValAssignment(name, compilationUnit, ast.name.position)
        }
      case None => throw UndefinedVariable(name, compilationUnit, ast.name.position)
    }
    UnitType
  }

  override protected def arrayAssignAST(ast: ArrayAssignAST): TopType =
    visit(ast.source) match {
      case ArrayType(arr: mutable.ArrayBuffer[TopType @unchecked]) =>
        visit(ast.index) match {
          case IntegerType(index) =>
            arr(index) = visit(ast.expr)
            UnitType
          case value =>
            throw TypeMismatch(value, Type.Integer, compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, Type.Array, compilationUnit, ast.source.token.position)
    }

  override protected def varAST(ast: VarAST): TopType =
    memory.get(ast.name.value) match {
      case Some(variable) => variable.value
      case None =>
        throw UndefinedVariable(ast.name.value, compilationUnit, ast.name.position)
    }

  override protected def program(ast: Program): TopType = {
    for (child <- ast.children
         if memory
           .get("return")
           .isEmpty && !memory.get("break").map(_.value).contains(BooleanType(true))) visit(child)
    UnitType
  }

  override protected def functionCall(ast: FunctionCall): TopType =
    visit(ast.source) match {
      case FunctionType(FunctionLiteral(formalParameters, body, _), closures) =>
        if (formalParameters.size != ast.actualParameters.size) {
          val fnName = ast.source match {
            case VarAST(name) => name.value
            case _            => "<anonymous>"
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
          closures.foreach {
            case (name, obj) =>
              memory.get(name) match {
                case Some(oldObj) =>
                  if (oldObj.scopeLevel < obj.scopeLevel) memory.assign(name -> obj.value)
                  else ()
                case None => memory.define(name -> obj.value, obj.isMutable)
              }
          }
          parameters.foreach(param => memory.define(param, isMutable = true))
          visit(body)
          val result = memory.get("return") match {
            case Some(ObjectInMemory(_, value, _, _)) => value
            case None                                 => UnitType
          }
          memory.popStack()
          result
        }
      case value =>
        throw TypeMismatch(value, Type.Function, compilationUnit, ast.source.token.position)
    }

  override protected def functionLiteral(ast: FunctionLiteral): TopType =
    FunctionType(ast, memory.getAll.toList)

  override protected def arrayAccess(ast: ArrayAccess): TopType =
    visit(ast.source) match {
      case ArrayType(ls: mutable.ArrayBuffer[TopType @unchecked]) =>
        visit(ast.index) match {
          case IntegerType(i) =>
            if (i >= 0 && i < ls.size) ls(i)
            else
              throw IndexOutOfBound(
                ast.token.value,
                ls.size,
                i,
                compilationUnit,
                ast.token.position
              )
          case value =>
            throw TypeMismatch(value, Type.Integer, compilationUnit, ast.source.token.position)
        }
      case value =>
        throw TypeMismatch(value, Type.Array, compilationUnit, ast.source.token.position)
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
      throw TypeMismatch(value, Type.Boolean, compilationUnit, ast.condition.token.position)
  }

  final override protected def whileAST(ast: WhileAST): TopType = {
    if (memory.get("return").nonEmpty) ()
    var BooleanType(condition) = visit(ast.condition) match {
      case b: BooleanType => b
      case value =>
        throw TypeMismatch(value, Type.Boolean, compilationUnit, ast.condition.token.position)
    }
    memory.pushNewLocalScope()
    while (condition
           && memory.get("return").isEmpty
           && !memory.get("break").map(_.value).contains(BooleanType(true))) {
      visit(ast.whileBlock)
      if (memory.get("return").nonEmpty
          || memory.get("break").map(_.value).contains(BooleanType(true))) {
        condition = false
      } else {
        val BooleanType(updated) = visit(ast.condition)
        condition = updated
      }
    }
    memory.assign("break" -> BooleanType(false))
    memory.popNewLocalScope()
    UnitType
  }

  override protected def returnAST(ast: ReturnAST): TopType =
    visit(
      ValDefAST(IdToken("return", ast.token.position), ast.result, inLoopBody = false, AssignToken(ast.token.position))
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
      mutable.Map(ast.elements.map {
        case (name, value) => (name.value, visit(value))
      }.toSeq: _*)
    )

  override protected def propertyAccess(ast: PropertyAccess): TopType = visit(ast.source) match {
    case ObjectType(objectLiteral: mutable.Map[String @unchecked, TopType @unchecked]) =>
      objectLiteral.get(ast.name.value) match {
        case Some(x) => x
        case None    => throw UndefinedVariable(ast.name.value, compilationUnit, ast.token.position)
      }
    case value =>
      throw TypeMismatch(value, Type.Object, compilationUnit, ast.source.token.position)
  }

  override protected def propertyAssign(ast: PropertyAssignAST): TopType =
    visit(ast.source) match {
      case ObjectType(map: mutable.Map[String @unchecked, TopType @unchecked]) =>
        map(ast.name.value) = visit(ast.expr)
        UnitType
      case value => throw TypeMismatch(value, Type.Object, compilationUnit, ast.source.token.position)

    }

  override protected def ifThenElseAST(ast: IfThenElseAST): TopType = visit(ast.condition) match {
    case BooleanType(true)  => visit(ast.ifBlock)
    case BooleanType(false) => visit(ast.elseBlock)
    case value =>
      throw TypeMismatch(value, Type.Boolean, compilationUnit, ast.condition.token.position)
  }

  override protected def importAST(ast: ImportAST): TopType = {
    val interpreter = new Interpreter()
    interpreter.runFromFile(ast.token.value + ".intp")
    // copy definitions after running interpreter
    interpreter.memory.getAll.foreach {
      case (name, obj) => memory.assign(name -> obj.value)
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
        throw TypeMismatch(value, Type.Boolean, compilationUnit, ast.condition.token.position)
    }
    while (condition
           && memory.get("return").isEmpty
           && !memory.get("break").map(_.value).contains(BooleanType(true))) {
      visit(ast.program)
      if (memory.get("return").nonEmpty
          || memory.get("break").map(_.value).contains(BooleanType(true))) {
        condition = false
      } else {
        ast.step.foreach(visit)
        val BooleanType(updated) = visit(ast.condition)
        condition = updated
      }
    }
    memory.assign("break" -> BooleanType(false))
    memory.popNewLocalScope()
    UnitType
  }

  override protected def breakAST(ast: BreakAST): TopType =
    visit(
      ValDefAST(
        IdToken("break", ast.token.position),
        BooleanLiteral(TrueToken(ast.token.position)),
        inLoopBody = true,
        AssignToken(ast.token.position)
      )
    )

  override protected def valDefAST(ast: ValDefAST): TopType = {
    val name = ast.name.value
    // repeated definition in loop's body are treated as assignments
    if (ast.inLoopBody && memory.get(name).nonEmpty) {
      visit(AssignAST(ast.name, ast.expr, AssignToken(ast.token.position)))
    } else {
      memory.get(name) match {
        case Some(objectInMemory) if objectInMemory.scopeLevel >= memory.scopeLevel =>
          throw ObjectRedefinition(name, compilationUnit, ast.token.position)
        case _ =>
          ast.expr match {
            // do not evaluate function definition
            case fl: FunctionLiteral =>
              memory.define(name -> FunctionType(fl, memory.getAll.toList), isMutable = false)
            case _ => memory.define(name -> visit(ast.expr), isMutable = false)
          }
      }
      UnitType
    }
  }

  override protected def varDefAST(ast: VarDefAST): TopType = {
    val name = ast.name.value
    // repeated definition in loop's body are treated as assignments
    if (ast.inLoopBody && memory.get(name).nonEmpty) {
      visit(AssignAST(ast.name, ast.expr, AssignToken(ast.token.position)))
    } else {
      memory.get(name) match {
        case Some(objectInMemory) if objectInMemory.scopeLevel >= memory.scopeLevel =>
          throw ObjectRedefinition(name, compilationUnit, ast.token.position)
        case _ =>
          ast.expr match {
            // do not evaluate function definition
            case fl: FunctionLiteral =>
              memory.define(name -> FunctionType(fl, memory.getAll.toList), isMutable = true)
            case _ => memory.define(name -> visit(ast.expr), isMutable = true)
          }
      }
      UnitType
    }
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
    compilationUnit = compilationUnit.copy(source = source)
    val parser = new Parser(source)(compilationUnit)
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

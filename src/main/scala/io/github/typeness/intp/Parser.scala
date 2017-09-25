package io.github.typeness.intp

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Parser(text: String)(val compilationUnit: CompilationUnit = CompilationUnit("<console>", text)) {
  private val lexer: Lexer = new Lexer(text)(compilationUnit)
  private var currentToken: Token = lexer.getNextToken

  def parse(): Program = {
    val node = program()
    if (currentToken.tokenType != EOF) {
      throw ParserError
    }
    //    val semanticAnalyzer = new SemanticAnalyzer()
    //    semanticAnalyzer.build(node)
    node
  }

  /*
  program = {statement} ;
   */
  private def program(): Program = {
    val statemets: ListBuffer[AST] = ListBuffer.empty
    while (currentToken.tokenType != EOF && currentToken.tokenType != R_CURLY_BRACKET) {
      statemets.append(statement())
    }
    Program(children = statemets.toList)
  }

  /*
  statement =   if_statement
             | while_statement
             | return_statement
             | disjunction ;
   */
  private def statement(): AST = {
    currentToken.tokenType match {
      case IF => ifStatement()
      case WHILE => whileStatement()
      case RETURN => returnStatement()
      case _ => disjunction()
    }
  }

  /*
 if_statement = IF L_ROUND_BRACKET disjunction R_ROUND_BRACKET L_CURLY_BRACKET program
      R_CURLY_BRACKET [ELSE (L_CURLY_BRACKET program R_CURLY_BRACKET) | if_statement] ;
  */
  private def ifStatement(): AST = {
    val ifTokenPos = currentToken.position
    eat(IF)
    eat(L_ROUND_BRACKET)
    val condition = disjunction()
    eat(R_ROUND_BRACKET)
    eat(L_CURLY_BRACKET)
    val ifBlock = program()
    eat(R_CURLY_BRACKET)
    val elseBlock = currentToken.tokenType match {
      case ELSE =>
        eat(ELSE)
        currentToken.tokenType match {
          case L_CURLY_BRACKET =>
            eat(L_CURLY_BRACKET)
            val elseBlock = program()
            eat(R_CURLY_BRACKET)
            Some(elseBlock)
          case IF => Some(ifStatement())
          case _ => throw SyntaxError(currentToken.value, compilationUnit, currentToken.position)
        }
//        eat(L_CURLY_BRACKET)
//        val elseBlock = program()
//        eat(R_CURLY_BRACKET)
//        Some(elseBlock)
      case _ => None
    }
    IfAST(condition, ifBlock, elseBlock, IfToken(ifTokenPos))
  }

  /*
 while_statement = WHILE L_ROUND_BRACKET disjunction R_ROUND_BRACKET L_CURLY_BRACKET program
      R_CURLY_BRACKET ;
  */
  private def whileStatement(): AST = {
    val pos = currentToken.position
    eat(WHILE)
    eat(L_ROUND_BRACKET)
    val condition = disjunction()
    eat(R_ROUND_BRACKET)
    eat(L_CURLY_BRACKET)
    val whileBlock = program()
    eat(R_CURLY_BRACKET)
    WhileAST(condition, whileBlock, WhileToken(pos))
  }

  /*
  return_statement = RETURN disjunction ;
   */
  private def returnStatement(): AST = {
    val pos = currentToken.position
    eat(RETURN)
    ReturnAST(result = disjunction(), ReturnToken(pos))
  }

  /*
  disjunction = conjunction ASSIGN disjunction
              | conjunction OR conjunction ;
   */
  private def disjunction(): AST = {
    var result = conjunction()
    // assignment
    if (currentToken.tokenType == ASSIGN) {
      val pos = currentToken.position
      eat(ASSIGN)
      result match {
        case ArrayAccess(name, index) =>
          ArrayAssignAST(name, index, disjunction())
        case VarAST(name) => AssignAST(name, disjunction(), AssignToken(pos))
        case _ => throw new ParserError(result.token)
      }
    } else {
      while (currentToken.tokenType == OR) {
        val op = currentToken
        eat(op.tokenType)
        result = BinOp(left = result, op = op, right = conjunction())
      }
      result
    }
  }

  /*
  conjunction = boolean
              | boolean AND boolean ;
   */
  private def conjunction(): AST = {
    var result = boolean()
    while (currentToken.tokenType == AND) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = boolean())
    }
    result
  }

  /*
  boolean = expression
          | expression GREATER expression
          | expression LESS expression
          | expression GREATER_OR_EQUALS expression
          | expression LESS_OR_EQUALS expression
          | expression EQUALS expression
          | expression NOT_EQUALS expression ;
   */
  private def boolean(): AST = {
    var result = expression()
    while (currentToken.tokenType == GREATER || currentToken.tokenType == LESS
      || currentToken.tokenType == GREATER_OR_EQUALS || currentToken.tokenType == LESS_OR_EQUALS
      || currentToken.tokenType == EQUALS || currentToken.tokenType == NOT_EQUALS) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = expression())
    }
    result
  }

  /*
  expression = term
            | term PLUS term
            | term MINUS term ;
   */
  private def expression(): AST = {
    var result = term()
    while (currentToken.tokenType == PLUS || currentToken.tokenType == MINUS) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = term())
    }
    result
  }

  /*
  term = factor
      | factor MULTIPLICATION factor
      | factor DIV factor
      | factor MODULO factor ;
   */
  private def term(): AST = {
    var result = factor()
    while (currentToken.tokenType == MULTIPLICATION
      || currentToken.tokenType == DIV || currentToken.tokenType == MODULO) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = factor())
    }
    result
  }

  /*
  factor = variable
        | function_literal
        | unary_operator factor
        | number_literal
        | boolean_literal
        | L_ROUND_BRACKET disjunction R_ROUND_BRACKET
        | array_literal
        | char_literal
        | string_literal ;
   */
  private def factor(): AST = {
    val token = currentToken
    currentToken.tokenType match {
      case ID => variable()
      case FUNC => functionLiteral()
      case PLUS | MINUS | NOT => unaryOperator()
      case INTEGER_CONST | REAL_CONST => numberLiteral()
      case TRUE | FALSE => booleanLiteral()
      case L_ROUND_BRACKET =>
        eat(L_ROUND_BRACKET)
        val result = disjunction()
        eat(R_ROUND_BRACKET)
        result
      case L_SQUARE_BRACKET => arrayLiteral()
      case APOSTROPHE => characterLiteral()
      case QUOTATION => stringLiteral()
      case _ => throw ParserError(token)
    }
  }

  /*
  variable = ID (actual_parameters_list)*
          | ID (array_indexing)*
          | ID ;
  */
  private def variable(): AST = {
    val name = currentToken.value
    val pos = currentToken.position
    eat(ID)
    currentToken.tokenType match {
      case L_ROUND_BRACKET =>
        var functionCall = FunctionCall(VarAST(IdToken(name, pos)), actualParametersList())
        while (currentToken.tokenType == L_ROUND_BRACKET) {
          functionCall = FunctionCall(source = functionCall, actualParametersList())
        }
        functionCall
      //        FunctionCall(source = VarAST(IdToken(name)), actualParametersList())
      case L_SQUARE_BRACKET =>
        var arrayAccess = ArrayAccess(VarAST(IdToken(name, pos)), arrayIndexing())
        while (currentToken.tokenType == L_SQUARE_BRACKET) {
          arrayAccess = ArrayAccess(source = arrayAccess, arrayIndexing())
        }
        arrayAccess
      case _ => VarAST(name = IdToken(name, pos))
    }
  }

  /*
  function_literal =
    FUNC formal_parameters_list L_CURLY_BRACKET program R_CURLY_BRACKET ;
   */
  private def functionLiteral(): AST = {
    val pos = currentToken.position
    eat(FUNC)
    val parameters = formalParametersList()
    eat(L_CURLY_BRACKET)
    val root = program()
    eat(R_CURLY_BRACKET)
    FunctionLiteral(parameters, root, FuncToken(pos))
  }

  /*
 unary_operator = PLUS factor
               | MINUS factor
               | NOT factor ;
  */
  private def unaryOperator(): AST = {
    currentToken.tokenType match {
      case PLUS | MINUS | NOT =>
        val token = currentToken
        eat(currentToken.tokenType)
        UnaryOp(op = token, expr = factor())
      case _ => throw new ParserError(currentToken)
    }
  }

  /*
  number_literal = INTEGER_CONST | REAL_CONST ;
   */
  private def numberLiteral(): AST = {
    val token = currentToken
    currentToken.tokenType match {
      case INTEGER_CONST =>
        eat(INTEGER_CONST)
        Number(IntegerConstToken(token.value.toInt, token.position))
      case REAL_CONST =>
        eat(REAL_CONST)
        Number(RealConstToken(token.value.toDouble, token.position))
      case _ => throw new ParserError(currentToken)
    }
  }

  /*
  boolean_literal = TRUE | FALSE ;
   */
  private def booleanLiteral(): AST = {
    val pos = currentToken.position
    currentToken.tokenType match {
      case TRUE =>
        eat(TRUE)
        BooleanLiteral(TrueToken(pos))
      case FALSE =>
        eat(FALSE)
        BooleanLiteral(FalseToken(pos))
      case _ => throw new ParserError(currentToken)
    }
  }

  /*
  def array_literal = L_SQUARE_BRACKET disjunction (COMMA disjunction)* R_SQUARE_BRACKET
                   | L_SQUARE_BRACKET R_SQUARE_BRACKET ;
   */
  private def arrayLiteral(): AST = {
    val pos = currentToken.position
    eat(L_SQUARE_BRACKET)
    currentToken.tokenType match {
      case R_SQUARE_BRACKET =>
        eat(R_SQUARE_BRACKET)
        ArrayLiteral(List.empty, LSquareBracketToken(pos))
      case _ =>
        val result: ListBuffer[AST] = ListBuffer(disjunction())
        while (currentToken.tokenType == COMMA) {
          eat(COMMA)
          result.append(disjunction())
        }
        eat(R_SQUARE_BRACKET)
        ArrayLiteral(result.toList, LSquareBracketToken(pos))
    }
  }

  /*
  character_literal = APOSTROPHE CHAR APOSTROPHE ;
   */

  private def characterLiteral(): AST = {
    eat(APOSTROPHE)
    currentToken match {
      case ch: CharToken =>
        val result = CharLiteral(ch)
        eat(CHARACTER)
        eat(APOSTROPHE)
        result
      case _ => throw new ParserError(currentToken)
    }
  }

  /*
  string_literal = QUOTATION STRING QUOTATION ;
   */

  private def stringLiteral(): AST = {
    val pos = currentToken.position
    eat(QUOTATION)
    currentToken match {
      case str: StringToken =>
        val result = ArrayLiteral(stringToListOfChars(str), LSquareBracketToken(pos))
        eat(STRING)
        eat(QUOTATION)
        result
      case _ => throw new ParserError(currentToken)
    }
  }

  /*
  actual_parameters_list = L_ROUND_BRACKET disjunction (COMMA disjunction)*
                  | L_ROUND_BRACKET R_ROUND_BRACKET ;
   */
  private def actualParametersList(): List[AST] = {
    eat(L_ROUND_BRACKET)
    currentToken.tokenType match {
      case R_ROUND_BRACKET =>
        eat(R_ROUND_BRACKET)
        List.empty
      case _ =>
        val result: ListBuffer[AST] = ListBuffer(disjunction())
        while (currentToken.tokenType == COMMA) {
          eat(COMMA)
          result.append(disjunction())
        }
        eat(R_ROUND_BRACKET)
        result.toList
    }
  }

  /*
  array_indexing = L_SQUARE_BRACKET expression R_SQUARE_BRACKET ;
   */
  private def arrayIndexing(): AST = {
    eat(L_SQUARE_BRACKET)
    val index = expression()
    eat(R_SQUARE_BRACKET)
    index
  }

  /*
  formal_parameters_list = L_ROUND_BRACKET ID (COMMA ID)* R_ROUND_BRACKET
                        | L_ROUND_BRACKET R_ROUND_BRACKET ;
   */
  private def formalParametersList(): List[IdToken] = {
    eat(L_ROUND_BRACKET)
    currentToken match {
      case token: IdToken =>
        val parameters: ListBuffer[IdToken] = ListBuffer(token)
        eat(ID)
        while (currentToken.tokenType == COMMA) {
          eat(COMMA)
          currentToken match {
            case token: IdToken =>
              parameters.append(token)
              eat(ID)
            case _ => throw ParserError
          }
        }
        eat(R_ROUND_BRACKET)
        parameters.toList
      case _ =>
        eat(R_ROUND_BRACKET)
        List.empty
    }
  }

  private def stringToListOfChars(str: StringToken): List[AST] =
    str.value.map(c => CharLiteral(CharToken(c, str.position))).toList

  private def eat(tokenType: TokenType): Unit = {
    if (currentToken.tokenType == tokenType) {
      currentToken = lexer.getNextToken
    } else {
      throw ParserError(currentToken)
    }
  }
}

object Parser {
  def fromFile(filename: String): Parser = {
    val src = Source.fromFile(filename).mkString
    new Parser(src)(CompilationUnit(filename, src))
  }

  def fromResource(filename: String): Parser = {
    val src = Source.fromResource(filename).mkString
    new Parser(src)(CompilationUnit(filename, src))
  }
}

case object ParserError extends Exception {
  override def getMessage: String = "Invalid syntax"
}

case class ParserError(token: Token) extends Exception {
  override def getMessage: String = s"Invalid syntax: $token"
}
package io.github.typeness.intp

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Parser(text: String) {
  private val lexer: Lexer = new Lexer(text)
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
  program: (statement)*
   */
  def program(): Program = {
    val statemets: ListBuffer[AST] = ListBuffer.empty
    while (currentToken.tokenType != EOF && currentToken.tokenType != R_CURLY_BRACKET) {
      statemets.append(statement())
    }
    Program(children = statemets.toList)
  }

  /*
  statement: expression
             | loop
             | condition
   */
  def statement(): AST = {
    disjunction()
  }

  /*
  expression: term
   */
  def expression(): AST = {
    var result = term()
    while (currentToken.tokenType == PLUS || currentToken.tokenType == MINUS) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = term())
    }
    result
  }

  /*
  term: factor
      | factor MULTIPLICATION factor
      | factor DIV factor
      | factor OR factor
      | factor AND factor
   */
  def term(): AST = {
    var result = factor()
    while (currentToken.tokenType == MULTIPLICATION || currentToken.tokenType == DIV) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = factor())
    }
    result
  }

  def boolean(): AST = {
    var result = expression()
    while (currentToken.tokenType == GREATER || currentToken.tokenType == LESS
      || currentToken.tokenType == GREATER_OR_EQUALS || currentToken.tokenType == LESS_OR_EQUALS
      || currentToken.tokenType == EQUALS) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = expression())
    }
    result
  }

  def conjunction(): AST = {
    var result = boolean()
    while (currentToken.tokenType == AND) {
      val op = currentToken
      eat(op.tokenType)
      result = BinOp(left = result, op = op, right = boolean())
    }
    result
  }

  def disjunction(): AST = {
    var result = conjunction()
    // assignment
    if (currentToken.tokenType == ASSIGN) {
      eat(ASSIGN)
      result match {
        case ArrayAccess(name, index) =>
          ArrayAssignAST(name, index, disjunction())
        case VarAST(name) => AssignAST(name, disjunction())
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
  array_indexing: L_SQUARE_BRACKET expression R_SQUARE_BRACKET
   */
  def arrayIndexing(): AST = {
    eat(L_SQUARE_BRACKET)
    val index = expression()
    eat(R_SQUARE_BRACKET)
    index
  }

  /*
  function_definition:
    FUNC formal_parameters_list L_CURLY_BRACKET program R_CURLY_BRACKET
   */
  def functionDefinition(): AST = {
    eat(FUNC)
    val parameters = formalParametersList()
    eat(L_CURLY_BRACKET)
    val root = program()
    eat(R_CURLY_BRACKET)
    FunctionDefinition(parameters, root)
  }

  /*
  formal_parameters_list: L_ROUND_BRACKET ID (COMMA ID)* R_ROUND_BRACKET
                        | L_ROUND_BRACKET R_ROUND_BRACKET
   */
  def formalParametersList(): List[IdToken] = {
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

  /*
  factor: variable
        | unary_operator factor
        | literal
        | L_ROUND_BRACKET expression R_ROUND_BRACKET
   */

  def factor(): AST = {
    val token = currentToken
    currentToken.tokenType match {
      case ID => variable()
      case FUNC => functionDefinition()
      case PLUS | MINUS | NOT =>
        eat(currentToken.tokenType)
        UnaryOp(op = token, expr = factor())
      case INTEGER_CONST =>
        eat(INTEGER_CONST)
        Number(IntegerConstToken(token.value.toInt))
      case REAL_CONST =>
        eat(REAL_CONST)
        Number(RealConstToken(token.value.toDouble))
      case TRUE =>
        eat(TRUE)
        Boolean(TrueToken)
      case FALSE =>
        eat(FALSE)
        Boolean(FalseToken)
      case L_ROUND_BRACKET =>
        eat(L_ROUND_BRACKET)
        val result = disjunction()
        eat(R_ROUND_BRACKET)
        result
      case L_SQUARE_BRACKET =>
        arrrayLiteral()
      case APOSTROPHE => ???
      case QUOTATION => ???
    }
  }

  /*
  actual_parameters_list: L_ROUND_BRACKET disjunction (COMMA disjunction)*
                  | L_ROUND_BRACKET R_ROUND_BRACKET
   */
  def actualParametersList(): List[AST] = {
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
  def array_literal: L_SQUARE_BRACKET disjunction (COMMA disjunction)* R_SQUARE_BRACKET
                   | L_SQUARE_BRACKET R_SQUARE_BRACKET
   */
  def arrrayLiteral(): AST = {
    eat(L_SQUARE_BRACKET)
    currentToken.tokenType match {
      case R_SQUARE_BRACKET =>
        eat(R_SQUARE_BRACKET)
        ArrayLiteral(List.empty)
      case _ =>
        val result: ListBuffer[AST] = ListBuffer(disjunction())
        while (currentToken.tokenType == COMMA) {
          eat(COMMA)
          result.append(disjunction())
        }
        eat(R_SQUARE_BRACKET)
        ArrayLiteral(result.toList)
    }
  }

  /*
  variable: ID
          | ID actual_parameters_list
   */
  private def variable(): AST = {
    val name = currentToken.value
    eat(ID)
    currentToken.tokenType match {
      case L_ROUND_BRACKET => FunctionCall(name = IdToken(name), actualParametersList())
      case L_SQUARE_BRACKET => ArrayAccess(name = IdToken(name), arrayIndexing())
      case _ => VarAST(name = IdToken(name))
    }
  }

  /*
  literal: number_literal
        | array_literal
        | boolean_literal
        | character_literal
        | string_literal
   */
  def literal(): AST = ???

  /*
  unary_operator: PLUS
                | MINUS
                | NOT
   */
  def unaryOperator(): AST = {
    currentToken.tokenType match {
      case PLUS | MINUS | NOT =>
        eat(currentToken.tokenType)
        UnaryOp(op = currentToken, expr = factor())
      case _ => throw new ParserError(currentToken)
    }
  }

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
    new Parser(src)
  }

  def fromResource(filename: String): Parser = {
    val src = Source.fromResource(filename).mkString
    new Parser(src)
  }
}

case object ParserError extends Exception {
  override def getMessage: String = "Invalid syntax"
}

case class ParserError(token: Token) extends Exception {
  override def getMessage: String = s"Invalid syntax: $token"
}
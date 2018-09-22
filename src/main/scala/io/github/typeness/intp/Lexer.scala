package io.github.typeness.intp

class Lexer(text: String)(
  val compilationUnit: CompilationUnit = CompilationUnit("<console>", text)
) {

  private var iterator: Int = 0
  private var currentChar: Option[Char] =
    if (text.nonEmpty) Some(text.charAt(iterator)) else None
  private var isOpeningQuote: Boolean = false
  private var isClosingQuote: Boolean = false
  private var isOpeningApostrophe: Boolean = false
  private var isClosingApostrophe: Boolean = false
  private var currentPosition: Position = Position(1, 0)

  private val asciiStringEscapes: Map[Char, Char] = Map(
    'n' -> '\n',
    'r' -> '\r',
    't' -> '\t',
    'b' -> '\b'
  )

  private val asciiCharEscaped: Map[Char, Char] =
    asciiStringEscapes ++
      Map(
        '0' -> 0.toChar
      )

  private def keywords(str: String, pos: Position): Option[Token] = str match {
    case "if"     => Some(IfToken(pos))
    case "else"   => Some(ElseToken(pos))
    case "while"  => Some(WhileToken(pos))
    case "import" => Some(ImportToken(pos))
    case "or"     => Some(OrToken(pos))
    case "and"    => Some(AndToken(pos))
    case "not"    => Some(NotToken(pos))
    case "true"   => Some(TrueToken(pos))
    case "false"  => Some(FalseToken(pos))
    case "break"  => Some(BreakToken(pos))
    case "return" => Some(ReturnToken(pos))
    case "func"   => Some(FuncToken(pos))
    case "then"   => Some(ThenToken(pos))
    case "data"   => Some(DataToken(pos))
    case "for"    => Some(ForToken(pos))
    case "val"    => Some(ValToken(pos))
    case "var"    => Some(VarToken(pos))
    case _        => None
  }

  def getNextToken: Token = {
    if (isOpeningQuote) {
      isOpeningQuote = false
      isClosingQuote = true
      str()
    } else if (isOpeningApostrophe) {
      isOpeningApostrophe = false
      isClosingApostrophe = true
      character()
    } else if (currentChar.isDefined) {
      skipWhitespaces()
      skipComments()
      if (currentChar.isDefined && currentChar.get.isDigit) {
        number()
      } else if (currentChar.contains('=') && peek().contains('=')) {
        advance()
        advance()
        EqualsToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('>') && peek().contains('=')) {
        advance()
        advance()
        GreaterOrEqualsToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('<') && peek().contains('=')) {
        advance()
        advance()
        LessOrEqualsToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('!') && peek().contains('=')) {
        advance()
        advance()
        NotEqualsToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('+') && peek().contains('=')) {
        advance()
        advance()
        CompoundAdditionToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('*') && peek().contains('=')) {
        advance()
        advance()
        CompoundMultiplicationToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('-') && peek().contains('=')) {
        advance()
        advance()
        CompoundSubtractionToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('/') && peek().contains('=')) {
        advance()
        advance()
        CompoundDivisionToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('%') && peek().contains('=')) {
        advance()
        advance()
        CompoundModuloToken(currentPosition.copy(col = currentPosition.col - 1))
      } else if (currentChar.contains('=')) {
        advance()
        AssignToken(currentPosition)
      } else if (currentChar.contains('%')) {
        advance()
        ModuloToken(currentPosition)
      } else if (currentChar.contains(',')) {
        advance()
        CommaToken(currentPosition)
      } else if (currentChar.contains('+')) {
        advance()
        AdditionToken(currentPosition)
      } else if (currentChar.contains('-')) {
        advance()
        SubtractionToken(currentPosition)
      } else if (currentChar.contains('*')) {
        advance()
        MultiplicationToken(currentPosition)
      } else if (currentChar.contains('/')) {
        advance()
        DivisionToken(currentPosition)
      } else if (currentChar.contains('(')) {
        advance()
        LRoundBracketToken(currentPosition)
      } else if (currentChar.contains(')')) {
        advance()
        RRoundBracketToken(currentPosition)
      } else if (currentChar.contains('[')) {
        advance()
        LSquareBracketToken(currentPosition)
      } else if (currentChar.contains(']')) {
        advance()
        RSquareBracketToken(currentPosition)
      } else if (currentChar.contains('{')) {
        advance()
        LCurlyBracketToken(currentPosition)
      } else if (currentChar.contains('}')) {
        advance()
        RCurlyBracketToken(currentPosition)
      } else if (currentChar.contains('>')) {
        advance()
        GreaterToken(currentPosition)
      } else if (currentChar.contains('<')) {
        advance()
        LessToken(currentPosition)
      } else if (currentChar.contains('\'')) {
        if (!isClosingApostrophe) isOpeningApostrophe = true
        else isClosingApostrophe = false
        advance()
        ApostropheToken(currentPosition)
      } else if (currentChar.contains('"')) {
        if (!isClosingQuote) isOpeningQuote = true
        else isClosingQuote = false
        advance()
        QuotationToken(currentPosition)
      } else if (currentChar.isDefined && (currentChar.get.isLetter || currentChar.get == '_')) {
        id()
      } else if (currentChar.contains('.')) {
        advance()
        DotToken(currentPosition)
      } else if (currentChar.contains(';')) {
        advance()
        SemicolonToken(currentPosition)
      } else if (currentChar.isEmpty) {
        EOFToken(currentPosition)
      } else {
        throw SyntaxError(currentChar.getOrElse("<EOF>").toString, compilationUnit, currentPosition)
      }
    } else {
      EOFToken(currentPosition)
    }
  }

  private def advance(): Unit = {
    iterator += 1
    currentPosition = currentPosition.copy(col = currentPosition.col + 1)
    if (iterator >= text.length) {
      currentChar = None
    } else {
      currentChar = Some(text.charAt(iterator))
    }
  }

  private def skipComments(): Unit = {
    while (currentChar.contains('/') && peek().contains('/')) {
      advance()
      advance()
      while (!currentChar.contains('\n') && currentChar.isDefined) {
        advance()
      }
      advance()
      currentPosition = Position(currentPosition.row + 1, -1)
    }
  }

  private def skipWhitespaces(): Unit = {
    while (currentChar.isDefined && (text.charAt(iterator) == ' '
           || text.charAt(iterator) == '\n')) {
      if (text.charAt(iterator) == '\n') {
        currentPosition = Position(currentPosition.row + 1, -1)
      }
      advance()
    }
  }

  private def number(): Token = {
    val result = new StringBuilder()
    val pos = Position(currentPosition.row, currentPosition.col + 1)
    while (currentChar.isDefined && currentChar.get.isDigit) {
      result.append(currentChar.get)
      advance()
    }
    if (currentChar.isDefined && currentChar.get == '.') {
      result.append(currentChar.get)
      advance()
      while (currentChar.isDefined && currentChar.get.isDigit) {
        result.append(currentChar.get)
        advance()
      }
      RealConstToken(result.toString.toDouble, pos)
    } else {
      IntegerConstToken(result.toString.toInt, pos)
    }
  }

  private def peek(): Option[Char] = {
    if (iterator + 1 >= text.length) None
    else Some(text.charAt(iterator + 1))
  }

  private def str(): Token = {
    val sb = new StringBuilder()
    val pos = Position(currentPosition.row, currentPosition.col + 1)
    while (currentChar.isDefined && !currentChar.contains('"')) {
      if (currentChar.get == '\\') {
        advance()
        sb.append(asciiStringEscapes.getOrElse(currentChar.get, currentChar.get))
        advance()
      } else {
        sb.append(currentChar.get)
        advance()
      }
    }
    StringToken(sb.toString(), pos)
  }

  private def character(): Token = {
    val pos = Position(currentPosition.row, currentPosition.col + 1)
    val ch = currentChar.get
    advance()
    if (ch == '\\') {
      val escaped = asciiCharEscaped.getOrElse(currentChar.get, currentChar.get)
      advance()
      CharToken(escaped, pos)
    } else {
      CharToken(ch, pos)
    }
  }

  // todo: return IdToken
  private def id(): Token = {
    val pos = Position(currentPosition.row, currentPosition.col + 1)
    val sb = new StringBuilder()
    while (currentChar.isDefined && (currentChar.get.isLetterOrDigit || currentChar.get == '_')) {
      sb.append(currentChar.get)
      advance()
    }
    val result = sb.toString
    keywords(result.toLowerCase, pos) match {
      case Some(keyword) => keyword
      case None          => IdToken(result, pos)
    }
  }

}

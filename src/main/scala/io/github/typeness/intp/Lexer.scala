package io.github.typeness.intp

class Lexer(text: String) {
  private var currentPosition: Int = 0
  private var currentChar: Option[Char] = Some(text.charAt(currentPosition))
  private var isOpeningQuote: Boolean = false
  private var isClosingQuote: Boolean = false
  private var isOpeningApostrophe: Boolean = false
  private var isClosingApostrophe: Boolean = false

  private val keywords: Map[String, Token] = Map(
    IfToken.value -> IfToken,
    ElseToken.value -> ElseToken,
    WhileToken.value -> WhileToken,
    ImportToken.value -> ImportToken,
    OrToken.value -> OrToken,
    AndToken.value -> AndToken,
    NotToken.value -> NotToken,
    TrueToken.value -> TrueToken,
    FalseToken.value -> FalseToken,
    BreakToken.value -> BreakToken,
    ReturnToken.value -> ReturnToken,
    FuncToken.value -> FuncToken
  )

  def getNextToken: Token = {
    if (isOpeningQuote) {
      isOpeningQuote = false
      isClosingQuote = true
      str()
    } else if(isOpeningApostrophe) {
      isOpeningApostrophe = false
      isClosingApostrophe = true
      character()
    } else if (currentChar.isDefined) {
      skipWhitespaces()
      if (currentChar.isDefined && currentChar.get.isDigit) {
        number()
      } else if (currentChar.contains('=') && peek().contains('=')) {
        advance()
        advance()
        EqualsToken
      } else if (currentChar.contains('>') && peek().contains('=')) {
        advance()
        advance()
        GreaterOrEqualsToken
      } else if (currentChar.contains('<') && peek().contains('=')) {
        advance()
        advance()
        LessOrEqualsToken
      } else if (currentChar.contains('=')) {
        advance()
        AssignToken
      } else if (currentChar.contains(',')) {
        advance()
        CommaToken
      } else if (currentChar.contains('+')) {
        advance()
        AdditionToken
      } else if (currentChar.contains('-')) {
        advance()
        SubtractionToken
      } else if (currentChar.contains('*')) {
        advance()
        MultiplicationToken
      } else if (currentChar.contains('/')) {
        advance()
        DivisionToken
      } else if (currentChar.contains('(')) {
        advance()
        LRoundBracketToken
      } else if (currentChar.contains(')')) {
        advance()
        RRoundBracketToken
      } else if (currentChar.contains('[')) {
        advance()
        LSquareBracketToken
      } else if (currentChar.contains(']')) {
        advance()
        RSquareBracketToken
      } else if (currentChar.contains('{')) {
        advance()
        LCurlyBracketToken
      } else if (currentChar.contains('}')) {
        advance()
        RCurlyBracketToken
      } else if (currentChar.contains('>')) {
        advance()
        GreaterToken
      } else if (currentChar.contains('<')) {
        advance()
        LessToken
      } else if (currentChar.contains('\'')) {
        if(!isClosingApostrophe) isOpeningApostrophe = true
        else isClosingApostrophe = false
        advance()
        ApostropheToken
      } else if (currentChar.contains('"')) {
        if (!isClosingQuote) isOpeningQuote = true
        else isClosingQuote = false
        advance()
        QuotationToken
      } else if (currentChar.isDefined && (currentChar.get.isLetter || currentChar.get == '_')) {
        id()
      } else if (currentChar.contains('.')) {
        advance()
        DotToken
      } else {
        //        throw new Exception(s"Error parsing input $currentChar")
        EOFToken
      }
    } else {
      EOFToken
    }
  }

  private def advance(): Unit = {
    currentPosition += 1
    if (currentPosition >= text.length) {
      currentChar = None
    } else {
      currentChar = Some(text.charAt(currentPosition))
    }
  }

  private def skipWhitespaces(): Unit = {
    while (currentChar.isDefined && (text.charAt(currentPosition) == ' '
      || text.charAt(currentPosition) == '\n')) {
      advance()
    }
  }

  private def number(): Token = {
    val result = new StringBuilder()
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
      RealConstToken(result.toString.toDouble)
    } else {
      IntegerConstToken(result.toString.toInt)
    }
  }

  private def peek(): Option[Char] = {
    if (currentPosition + 1 >= text.length) None
    else Some(text.charAt(currentPosition + 1))
  }

  private def str(): Token = {
    val sb = new StringBuilder()
    var specialChar = false
    while (currentChar.isDefined && (specialChar || !currentChar.contains('"'))) {
      if (currentChar.get == '\\') {
        specialChar = true
      } else {
        specialChar = false
      }
      sb.append(currentChar.get)
      advance()
    }
    StringToken(sb.toString())
  }

  private def character(): Token = {
    val ch = currentChar.get
    advance()
    CharToken(ch)
  }

  // todo: return IdToken
  private def id(): Token = {
    val sb = new StringBuilder()
    while (currentChar.isDefined && (currentChar.get.isLetterOrDigit || currentChar.get == '_')) {
      sb.append(currentChar.get)
      advance()
    }
    val result = sb.toString
    keywords.get(result.toLowerCase) match {
      case Some(keyword) => keyword
      case None => IdToken(result)
    }
  }

}

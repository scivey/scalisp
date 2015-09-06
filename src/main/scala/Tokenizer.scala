package net.scivey.scalisp.tokenizer

abstract class Token
case object LParToken extends Token
case object RParToken extends Token
case class StrLitToken(v: String) extends Token
case class IntLitToken(v: Int) extends Token
// case class FloatLitToken(v: Float) extends Token
case class SymbolToken(v: String) extends Token

object Tokenizer {
  def isNumericChar(c: Char): Boolean = {
    val byteVal = c.toByte
    byteVal >= 48 && byteVal <= 57
  }
  def isAlphaChar(c: Char): Boolean = {
    val byteVal = c.toByte
    (byteVal >= 65 && byteVal <= 90) || (byteVal >= 97 && byteVal <= 122)
  }
  def isAlphanumeric(c: Char): Boolean = {
    isNumericChar(c) || isAlphaChar(c)
  }
  def isSymbolChar(c: Char): Boolean = {
    isAlphanumeric(c) || c == '_' || c == '-' || c == '+' || c == '*' || c == '/'
  }
  def takeSymbolToken(source: String): (Option[Token], String) = {
    if (!isSymbolChar(source.head)) {
      return (None, source)
    }
    var c = source.head
    var remainingSource = source.substring(1)
    var symbolStr = ""
    while (isSymbolChar(c) && remainingSource.length > 0) {
      symbolStr += c
      c = remainingSource.head
      remainingSource = remainingSource.substring(1)
    }
    return (Some(SymbolToken(symbolStr)), remainingSource)
  }
  def takeIntToken(source: String): (Option[Token], String) = {
    if (!isNumericChar(source.head)) {
      return (None, source)
    }
    var c = source.head
    var remainingSource = source.substring(1)
    var intStr = ""
    while (isNumericChar(c) && remainingSource.length > 0) {
      intStr += c
      c = remainingSource.head
      remainingSource = remainingSource.substring(1)
    }
    return (Some(IntLitToken(Integer.parseInt(intStr))), remainingSource)
  }
  def tokenizeRec(source: String, tokens: Seq[Token]): Seq[Token] = {
    if (source.length == 0) {
      return tokens
    }
    source.head match {
      case '(' => tokenizeRec(source.substring(1), tokens ++ Seq(LParToken))
      case ')' => tokenizeRec(source.substring(1), tokens ++ Seq(RParToken))
      case ' ' => tokenizeRec(source.substring(1), tokens)
      case '\n' => tokenizeRec(source.substring(1), tokens)
      case '\t' => tokenizeRec(source.substring(1), tokens)
      case '"' => {
        val remaining = source.substring(1)
        val litEnd = remaining.indexOf('"')
        val tokeVal = remaining.subSequence(0, litEnd).toString
        tokenizeRec(remaining.substring(litEnd + 1), tokens ++ Seq(StrLitToken(tokeVal)))
      }
      case _ => {
        val (toke, remaining) = takeIntToken(source)
        toke match {
          case Some(t) => {
            tokenizeRec(remaining, tokens ++ Seq(t))
          }
          case None => {
            val (toke2, remaining2) = takeSymbolToken(source)
            toke2 match {
              case Some(t) => {
                tokenizeRec(remaining2, tokens ++ Seq(t))
              }
              case None => {
                println("bad token?")
                tokenizeRec(source.substring(1), tokens)
              }
            }
          }
        }
      }
    }
  }
  def tokenize(source: String): Seq[Token] = {
    val tokens = Seq[Token]()
    tokenizeRec(source, tokens)
  }
}
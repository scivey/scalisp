package net.scivey.scalisp.parser
import net.scivey.scalisp.tokenizer._
import net.scivey.scalisp.ast._
import scala.util.control.Breaks._

class ParseError (msg: String) extends RuntimeException(msg)

object Parser {
  def parseLambda(tokens: Seq[Token]): (Term, Seq[Token]) = {
    val isLPar = (t: Token) => {
      t match {
        case LParToken => true
        case _ => false
      }
    }
    if (!isLPar(tokens.head)) {
      throw new ParseError("expected LPar (lambda params)")
    }
    var (params, remaining) = parseSexpr(tokens.slice(1, tokens.length))

    if (!isLPar(remaining.head)) {
      throw new ParseError("expected LPar (lambda body)")
    }

    var (body, rem) = parseSexpr(remaining.slice(1, tokens.length))

    var symTerms: Seq[Symbol] = params match {
      case TermList(terms) => {
        terms.map { t =>
          t match {
            case (s: Symbol) => s
            case _ => throw new ParseError("expected symbol")
          }
        }
      }
      case _ => throw new ParseError("expected TermList")
    }
    (Func(symTerms, body, None), rem)
  }
  def parseSexpr(tokens: Seq[Token]): (Term, Seq[Token]) = {
    val notRPar = (t: Token) => {
      t match {
        case RParToken => false
        case _ => true
      }
    }
    var current: Token = tokens.head
    var remaining = tokens
    var terms: Seq[Term] = Seq[Term]()
    breakable {
      while (remaining.length > 0) {
        if (notRPar(remaining.head)) {
          val (nextTerm, rem) = parseRec(remaining)
          terms = terms ++ Seq(nextTerm)
          remaining = rem
        } else {
          break
        }
      }
    }
    (TermList(terms), remaining.slice(1, remaining.length))
  }
  def parseRec(tokens: Seq[Token]): (Term, Seq[Token]) = {
    if (tokens.length == 0) {
      tokens
    }
    tokens.head match {
      case IntLitToken(i) => (IntLit(i), tokens.slice(1, tokens.length))
      case StrLitToken(s) => (StrLit(s), tokens.slice(1, tokens.length))
      case SymbolToken(s) => {
        s match {
          case "lambda" => {
            parseLambda(tokens.slice(1, tokens.length))
          }
          case "define" => {
            (Define, tokens.slice(1, tokens.length))
          }
          case "do" => {
            (Do, tokens.slice(1, tokens.length))
          }
          case "if" => {
            (IfExpr, tokens.slice(1, tokens.length))
          }
          case "true" => {
            (BoolLit(true), tokens.slice(1, tokens.length))
          }
          case "false" => {
            (BoolLit(false), tokens.slice(1, tokens.length))
          }
          case s => {
            (Symbol(s), tokens.slice(1, tokens.length))
          }
        }
      }
      case LParToken => parseSexpr(tokens.slice(1, tokens.length))
      case RParToken => throw new ParseError("bad")
    }
  }
  def parse(tokens: Seq[Token]): Term = {
    val (term, remaining) = parseRec(tokens)
    term
  }
}
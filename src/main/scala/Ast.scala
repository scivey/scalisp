package net.scivey.scalisp.ast
import scala.collection.mutable.Map
import net.scivey.scalisp.{ScopeError, Builtins, TypeError, Scope}

abstract class Term

case class TermList(terms: Seq[Term]) extends Term

abstract class Literal extends Term

case class IntLit(v: Int) extends Literal {
  override def toString(): String = v.toString
}

case class StrLit(v: String) extends Literal {
  override def toString(): String = "\"" + v + "\""
}

case class FloatLit(v: Float) extends Literal {
  override def toString(): String = v.toString
}

case class BoolLit(v: Boolean) extends Literal {
  override def toString(): String = v.toString
}

case object NilVal extends Literal {
  override def toString(): String = "Nil"
}

case class Symbol(name: String) extends Term {
  override def toString(): String = "Symbol('" + name + "')"
}

case class Func(params: Seq[Symbol], body: Term, context: Option[Scope]) extends Term

abstract class Builtin extends Term

case class BuiltinFunc(func: Seq[Term] => Term) extends Builtin

case object Let extends Builtin
case object Define extends Builtin
case object IfExpr extends Builtin
case object Do extends Builtin

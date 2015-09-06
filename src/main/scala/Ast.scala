package net.scivey.scalisp.ast
import scala.collection.mutable.Map
import net.scivey.scalisp.ScopeError


class Scope(
  val bindings: Map[Symbol, Term],
  val parent: Option[Scope]
){
  def push(newBindings: Map[Symbol, Term]): Scope = {
    new Scope(
      bindings ++ newBindings,
      Some(this)
    )
  }
  def push(subScope: Scope): Scope = {
    new Scope(
      bindings ++ subScope.bindings,
      Some(this)
    )
  }
  def get(s: Symbol): Term = {
    if (bindings.contains(s)) {
      return bindings(s)
    }
    parent match {
      case Some(scope) => scope.get(s)
      case None => throw new ScopeError("symbol not in scope: " + s.name)
    }
  }
  override def toString(): String = {
    "Scope( " + bindings.toString + ")"
  }
}

object Scope {
  val rootBindings: Map[Symbol, Term] = Map(
    Symbol("define") -> Define,
    Symbol("let") -> Let,
    Symbol("cons") -> Cons,
    Symbol("list") -> MkList,
    Symbol("print") -> Print,
    Symbol("+") -> Add,
    Symbol("*") -> Mul
  )

  def root(): Scope = {
    new Scope(rootBindings, None)
  }
}

abstract class Term

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
case object NilVal extends Literal {
  override def toString(): String = "Nil"
}

case class Symbol(name: String) extends Term {
  override def toString(): String = "Symbol('" + name + "')"
}


case class TermList(terms: Seq[Term]) extends Term

case class Func(params: Seq[Symbol], body: Term, context: Option[Scope]) extends Term

abstract class Builtin extends Term
case object Cons extends Builtin
case object Let extends Builtin
case object Define extends Builtin
case object Print extends Builtin
case object MkList extends Builtin
case object IfExp extends Builtin
case object Add extends Builtin
case object Mul extends Builtin

case object Do extends Builtin


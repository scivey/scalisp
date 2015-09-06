package net.scivey.scalisp
import scala.collection.mutable.Map
import net.scivey.scalisp.ast._

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
    "Scope"
  }
}

object Scope {

  val add2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.add2(terms(0), terms(1))
  })

  val mul2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.mul2(terms(0), terms(1))
  })

  val gt2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.gt2(terms(0), terms(1))
  })

  val lt2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.lt2(terms(0), terms(1))
  })

  val builtinPrint = BuiltinFunc({ terms: Seq[Term] =>
    println(terms(0))
    NilVal
  })

  val builtinCar = BuiltinFunc({ terms: Seq[Term] =>
    terms(0) match {
      case TermList(children) => children.head
      case _ => throw new TypeError("car expects a list")
    }
  })

  val builtinCdr = BuiltinFunc({ terms: Seq[Term] =>
    terms(0) match {
      case TermList(children) => TermList(children.tail)
      case _ => throw new TypeError("car expects a list")
    }
  })

  val builtinList = BuiltinFunc({ terms: Seq[Term] =>
    TermList(terms)
  })

  val builtinTruthy = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.isTruthy(terms(0))
  })

  val builtinIsNil = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.isNil(terms(0))
  })

  val builtinGetLen = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.getListLen(terms(0))
  })

  val rootBindings: Map[Symbol, Term] = Map(
    Symbol("define") -> Define,
    Symbol("let") -> Let,
    Symbol("print") -> builtinPrint,
    Symbol("+") -> add2,
    Symbol("*") -> mul2,
    Symbol(">") -> gt2,
    Symbol("<") -> lt2,
    Symbol("car") -> builtinCar,
    Symbol("cdr") -> builtinCdr,
    Symbol("list") -> builtinList,
    Symbol("truthy?") -> builtinTruthy,
    Symbol("nil?") -> builtinIsNil,
    Symbol("nil") -> NilVal,
    Symbol("len") -> builtinGetLen
  )

  def root(): Scope = {
    new Scope(rootBindings, None)
  }
}

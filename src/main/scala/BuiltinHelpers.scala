package net.scivey.scalisp

import net.scivey.scalisp.ast._

object BuiltinHelpers {
  def isTruthy(t: Term): Boolean = {
    t match {
      case NilVal => false
      case BoolLit(v) => {
        v
      }
      case IntLit(i) => {
        if (i == 0) {
          false
        } else {
          true
        }
      }
      case StrLit(s) => {
        if (s == "") {
          false
        } else {
          true
        }
      }
      case TermList(terms) => {
        if (terms.length > 0) {
          true
        } else {
          false
        }
      }
      case _ => throw new TypeError("Undefined truthiness for term: " + t)
    }
  }
  def isNil(t: Term): Boolean = {
    t match {
      case NilVal => true
      case _ => false
    }
  }
  def getListLen(t: Term): Int = {
    t match {
      case TermList(terms) => terms.length
      case _ => throw new TypeError("len expects a list argument.")
    }
  }
}


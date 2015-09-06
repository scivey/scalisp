package net.scivey.scalisp

class InterpreterException(msg: String) extends RuntimeException(msg)
class MalformedException(msg: String) extends InterpreterException(msg)
class TypeError(msg: String) extends InterpreterException(msg)
class ScopeError(msg: String) extends InterpreterException(msg)

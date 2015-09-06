package net.scivey.scalisp.main
import net.scivey.scalisp.ast._
import net.scivey.scalisp.Interpreter
import net.scivey.scalisp.tokenizer.Tokenizer
import net.scivey.scalisp.parser.Parser



object Main {
  val prog = TermList(
    Seq(
      TermList(
        Seq(
          Symbol("define"),
          Symbol("printTwice"),
          Func(
            Seq(Symbol("x")),
            TermList(
              Seq(
                TermList(Seq(Symbol("print"), Symbol("x"))),
                TermList(Seq(Symbol("print"), Symbol("x")))
              )
            ),
            None
          )
        )
      ),
      TermList(
        Seq(
          Symbol("printTwice"),
          StrLit("message1")
        )
      ),
      TermList(
        Seq(
          Symbol("print"),
          StrLit("message2")
        )
      )
    )
  )

  val source = """
    (
      ( define printTwice lambda
        ( x )
        ( do
          ( print x )
          ( print x )
        )
      )
      ( define addby lambda
        ( x )
        ( lambda
          ( y )
          ( do
            ( + x y )
          )
        )
      )
      ( define add10 ( addby 10 ) )
      ( define add50 ( addby 50 ) )
      ( printTwice "something" )
      ( printTwice ( add10 7 ) )
      ( printTwice ( add50 3 ) )
    )
  """

  def main(args: Array[String]) = {
    println("start")
    // Interpreter.evaluate(prog)
    val tokes = Tokenizer.tokenize(source)
    println(tokes)
    println(prog)
    val prog2 = Parser.parse(tokes)
    println(prog2)
    Interpreter.evaluate(prog2)

    println("stop")
  }
}
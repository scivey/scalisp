package net.scivey.scalisp.main
import net.scivey.scalisp.ast._
import net.scivey.scalisp.Interpreter
import net.scivey.scalisp.tokenizer.Tokenizer
import net.scivey.scalisp.parser.Parser



object Main {
  def main(args: Array[String]): Unit = {
    val prog = Parser.parse(Tokenizer.tokenize(source))
    println(prog)
    Interpreter.evaluate(prog)
  }

  val source = """
    (
      ( define printTwice
        (lambda
          ( x )
          ( do
            ( print x )
            ( print x )
          )
        )
      )
      ( define isBetween
        ( lambda
          ( x y )
          ( lambda
            ( z )
            ( if
              ( < z x )
              false
              ( if
                ( > z y )
                false
                true
              )
            )
          )
        )
      )
      ( define addby
        ( lambda
          ( x )
          ( lambda
            ( y )
            ( + x y )
          )
        )
      )
      ( define lessThan
        ( lambda
          ( y )
          ( lambda
            ( x )
            ( if
              ( < x y )
              true
              false
            )
          )
        )
      )
      ( define sayIfGreater
        ( lambda
          ( y )
          ( lambda
            ( x )
            ( if
              ( > x y )
              ( print "is greater" )
              ( print "is not greater" )
            )
          )
        )
      )
      ( define add10 ( addby 10 ) )
      ( define add50 ( addby 50 ) )
      ( printTwice "something" )
      ( print ( add10 7 ) )
      ( print ( add50 3 ) )
      ( define gt7 ( sayIfGreater 7 ) )
      ( gt7 3 )
      ( gt7 10 )
      ( ( sayIfGreater 20 ) 30 )
      ( gt7 19 )
      ( let
        ( z 19 ab 7 )
        ( do
          ( print z )
          ( print ab )
          ( printTwice z )
        )
      )
      ( define between30And65 ( isBetween 30 65 ) )
      ( print "between: start" )
      ( print ( between30And65 40 ) )
      ( print ( between30And65 20 ) )
      ( print ( between30And65 50 ) )
      ( print ( between30And65 100 ) )
      ( print ( between30And65 60 ) )
      ( print "between: end" )
      ( define lt30 ( lessThan 30 ) )
      ( define cx ( ( lessThan 30 ) 10 ) )
      ( define cy ( ( lessThan 30 ) 60 ) )
      ( print "one" )
      ( print ( lt30 10 ) )
      ( print "two" )
      ( print ( lt30 60 ) )
      ( let
        ( zz ( lessThan 30 ) )
        ( let
          ( xx ( zz 10 ) )
          ( do
            ( print "one" )
            ( print xx )
            ( print "two" )
          )
        )
      )
      ( print "end1" )
      ( let
        ( cz ( ( lessThan 30 ) 10 ) )
        ( print cz )
      )
      ( print "end2" )
      ( print ( car ( list 5 4 3 ) ) )
      ( print ( cdr ( list 5 4 3 ) ) )
      ( define printer
        ( lambda
          ( x )
          ( lambda
            ( )
            ( print x )
          )
        )
      )
      ( define sayHello ( printer "hello" ) )
      ( sayHello )
      ( define curry2
        ( lambda
          ( f )
          ( lambda
            ( x )
            ( lambda
              ( y )
              ( f x y )
            )
          )
        )
      )
      ( define partial2
        ( lambda
          ( f x )
          ( ( curry2 f ) x )
        )
      )
      ( define add5 ( ( curry2 + ) 5 ) )
      ( print ( add5 7 ) )
      ( define add60 ( partial2 + 60 ) )
      ( print ( add60 3 ) )
      ( print ( truthy? nil ) )
      ( print ( truthy? 5 ) )
      ( print ( truthy? 5 ) )
      ( print ( nil? nil ) )
      ( print ( nil? 0 ) )
      ( print ( nil? nil ) )
      ( print ( len ( list 5 4 2 ) ) )
      ( print ( len ( list 2 ) ) )
    )
  """
}

package javainterpreter.javalexer
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.JavaTokenParsers

trait JavaCompilationError
case class JavaLexerError(msg: String) extends JavaCompilationError


sealed trait JavaToken extends Positional

case class IDENTIFIER (str: String)   extends JavaToken
case class STRLITERAL (str: String)   extends JavaToken
case class NUMLITERAL (value: Double) extends JavaToken
//case class INTLITERAL (value: Int) extends JavaToken
case class TYPE(str: String) extends JavaToken


/* Keywords */

// Access Control
case class PRIVATE()   extends JavaToken
case class PROTECTED() extends JavaToken
case class PUBLIC()    extends JavaToken

// Class, Method, Varible Modifier
case class ABSTRACT()  extends JavaToken
case class CLASS()     extends JavaToken
case class EXTENDS()   extends JavaToken
case class FINAL()     extends JavaToken
case class IMPLEMENTS()extends JavaToken
case class INTERFACE() extends JavaToken
case class NATIVE()    extends JavaToken
case class NEW()       extends JavaToken
case class STATIC()    extends JavaToken

// Control Statement
case class BREAK()     extends JavaToken
case class CASE()      extends JavaToken
case class CONTINUE()  extends JavaToken
case class DEFAULT()   extends JavaToken
case class DO()        extends JavaToken
case class ELSE()      extends JavaToken
case class FOR()       extends JavaToken
case class IF()        extends JavaToken
case class INSTANCEOF()extends JavaToken
case class RETURN()    extends JavaToken
case class SWITCH()    extends JavaToken
case class WHILE()     extends JavaToken


case class PRINT() extends JavaToken
// Error Control
case class ASSERT()  extends JavaToken
case class CATCH()   extends JavaToken
case class FINALLY() extends JavaToken
case class THROW()   extends JavaToken
case class THROWS()  extends JavaToken
case class TRY()     extends JavaToken

// Package Related
case class IMPORT()  extends JavaToken
case class PACKAGE() extends JavaToken


// Basic Type
case class BOOLEAN() extends JavaToken
case class CHAR()    extends JavaToken
case class STRING()  extends JavaToken
case class DOUBLE()  extends JavaToken
case class FLOAT()   extends JavaToken
case class INT()     extends JavaToken
case class LONG()    extends JavaToken
case class SHORT()   extends JavaToken

// Varible Reference
case class SUPER()   extends JavaToken
case class THIS()    extends JavaToken
case class VOID()    extends JavaToken

// Reserved Keyword
case class GOTO()    extends JavaToken
case class CONST()   extends JavaToken
case class NULL()    extends JavaToken

/*symbol*/
//arith
case class ADD() extends JavaToken // "+"
case class SUB() extends JavaToken // "-"
case class MUL() extends JavaToken // "*"
case class DIV() extends JavaToken // "/"
case class MOD() extends JavaToken // "%"
case class INC() extends JavaToken // "++"
case class DEC() extends JavaToken // "--"

// boolean
case class EQUALS()      extends JavaToken // "=="
case class NOTEQUAL()    extends JavaToken // "!="
case class GREATERTHAN() extends JavaToken // ">"
case class GREATEREQUALS()extends JavaToken // ">="
case class SMALLERTHAN() extends JavaToken // "<"
case class SMALLEREQUALS()extends JavaToken // "<="

// logic
case class AND()    extends JavaToken // "&&"
case class OR()     extends JavaToken // "||"
case class NOT()    extends JavaToken // "!"

//assign
case class EQUAL()       extends JavaToken // "="
case class ADDEQUAL()    extends JavaToken // "+="
case class SUBEQUAL()    extends JavaToken // "-="
case class MULEQUAL()    extends JavaToken // "*="
case class DIVEQUAL()    extends JavaToken // "/="
case class MODEQUAL()    extends JavaToken // "%="




case class SEMICOLON()   extends JavaToken // ";"
case class COLON()       extends JavaToken
case class LEFTBRACKET() extends JavaToken // "("
case class RIGHTBRACKET()extends JavaToken // ")"
case class LEFTBRACE()   extends JavaToken // "{"
case class RIGHTBRACE()  extends JavaToken // "}"



object JavaLexer extends JavaTokenParsers{

    def apply(code: String): Either[JavaLexerError, List[JavaToken]] = {
      parse(tokens, code) match {
        case NoSuccess(msg, next) => Left(JavaLexerError(msg))
        case Success(result, next) => Right(result)
      }
    }

    def number : Parser[NUMLITERAL] = """\d+(\.\d*)?""".r^^{
      num => NUMLITERAL(num.toDouble)
    }
    
    // def intnumber : Parser[INTLITERAL] = """\d+(\d*)?""".r^^{
    //   num => INTLITERAL(num.toInt)
    // }  

    def literal: Parser[STRLITERAL] = positioned {
      """"[^"]*"""".r ^^ { str =>
        val content = str.substring(1, str.length - 1)
        STRLITERAL(content)
      }
    }
    

    def identifier: Parser[IDENTIFIER] = positioned {
        "[a-zA-Z_$][a-zA-Z0-9_$]*".r ^^ { str => IDENTIFIER(str)}
    }


    def tokens: Parser[List[JavaToken]] = {
      phrase(rep1(printout| int | double| string | switchcond | casecond | ifcondition | whilecondition | forcondition | elsecondition | addequal | subequal | mulequal | divequal | modequal | inc | dec | add | sub | mul 
                | div | mod | notequal | not | and | or | equals | equal | semicolon | leftbracket | rightbracket 
                | greaterequal| smallerequal| greaterthan | smallerthan| leftbrace | colon|breakout
                | rightbrace | number | literal | identifier)) 
    }

    def int          = positioned { "int"    ^^   (_ => INT()) }
    def double       = positioned { "double" ^^   (_ => DOUBLE())}
    def ifcondition  = positioned { "if"     ^^   (_ => IF()) }
    def whilecondition=positioned { "while"  ^^   (_ => WHILE())}
    def forcondition = positioned { "for"    ^^   (_ => FOR())}
    def elsecondition= positioned { "else"   ^^   (_ => ELSE())}
    def switchcond   = positioned { "switch" ^^   (_ => SWITCH())}
    def casecond     = positioned { "case"   ^^   (_ => CASE())}
    def addequal     = positioned { "+="     ^^   (_ => ADDEQUAL()) }
    def subequal     = positioned { "-="     ^^   (_ => SUBEQUAL()) }
    def mulequal     = positioned { "*="     ^^   (_ => MULEQUAL()) }
    def divequal     = positioned { "/="     ^^   (_ => DIVEQUAL()) }
    def modequal     = positioned { "%="     ^^   (_ => MODEQUAL()) }
    def add          = positioned { "+"      ^^   (_ => ADD()) }
    def sub          = positioned { "-"      ^^   (_ => SUB()) }
    def mul          = positioned { "*"      ^^   (_ => MUL()) }
    def div          = positioned { "/"      ^^   (_ => DIV()) }
    def mod          = positioned { "%"      ^^   (_ => MOD()) }
    def inc          = positioned { "++"     ^^   (_ => INC()) }
    def dec          = positioned { "--"     ^^   (_ => DEC()) }
    def and          = positioned { "&&"     ^^   (_ => AND()) }
    def or           = positioned { "||"     ^^   (_ => OR()) }
    def notequal     = positioned { "!="     ^^   (_ => NOTEQUAL())}
    def not          = positioned { "!"      ^^   (_ => NOT()) }
    def equal        = positioned { "="      ^^   (_ => EQUAL())}
    def equals       = positioned { "=="     ^^   (_ => EQUALS()) }
    def string       = positioned { "String" ^^   (_ => STRING())}   
    def semicolon    = positioned { ";"      ^^   (_ => SEMICOLON()) }
    def colon        = positioned { ":"      ^^   (_ => COLON())}
    def leftbracket  = positioned { "("      ^^   (_ => LEFTBRACKET()) }
    def rightbracket = positioned { ")"      ^^   (_ => RIGHTBRACKET()) }
    def greaterthan  = positioned { ">"      ^^   (_ => GREATERTHAN()) }
    def greaterequal = positioned { ">="     ^^   (_ => GREATEREQUALS()) }
    def smallerthan  = positioned { "<"      ^^   (_ => SMALLERTHAN())}
    def smallerequal = positioned { "<="     ^^   (_ => SMALLEREQUALS())}
    def leftbrace    = positioned { "{"      ^^   (_ => LEFTBRACE()) }
    def rightbrace   = positioned { "}"      ^^   (_ => RIGHTBRACE()) }
    def breakout     = positioned { "break"  ^^   (_ => BREAK())}
    def printout     = positioned { "System.out.println" ^^ (_ => PRINT())}




}

/*val code = 
 """
 int a = 33;
 int b = 5;
 if (a > 5){
  b = 10;
 }
 """

val code2 = """a = 33.0 """

println(JavaLexer.apply(code))
*/

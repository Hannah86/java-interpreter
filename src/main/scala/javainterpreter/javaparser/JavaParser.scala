package javainterpreter.javaparser

import javainterpreter.javacompiler.{Location, JavaParserError}
import javainterpreter.javalexer._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}


object JavaParser extends Parsers{
	override type Elem = JavaToken

	class JavaTokenReader(tokens: Seq[JavaToken]) extends Reader[JavaToken] {
	    override def first: JavaToken = tokens.head
	    override def atEnd: Boolean = tokens.isEmpty
	    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
	    override def rest: Reader[JavaToken] = new JavaTokenReader(tokens.tail)
	}

	def apply(tokens: Seq[JavaToken]): Either[JavaParserError, JavaAST] = {
	    val reader = new JavaTokenReader(tokens)
	    program(reader) match {
	    	case NoSuccess(msg, next) => Left(JavaParserError(Location(next.pos.line, next.pos.column), msg))
	      	case Success(result, next) => Right(result)
    	}
	}


    def program: Parser[JavaAST] = positioned {
    	phrase(block)
  	}

  	def block: Parser[JavaAST] = positioned {
	    rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
	}


	def statement: Parser[JavaAST] = positioned {
		val declareStr = STRING() ~ identifier ~ EQUAL() ~ strLiteral ~ SEMICOLON() ^^ {
			case _ ~ IDENTIFIER(name) ~ _ ~ STRLITERAL(value) ~ _ => DeclareStr(name, StrExpr(value)) 
		}  
		val declareInt = INT() ~ identifier ~ EQUAL() ~ count ~ SEMICOLON() ^^{
			case _ ~ IDENTIFIER(name) ~ _ ~ answer ~ _ => DeclareNum(name, answer)
		}
		val declareDouble = DOUBLE() ~ identifier ~ EQUAL() ~ count ~ SEMICOLON() ^^{
			case _ ~ IDENTIFIER(name) ~ _ ~ answer ~ _ => DeclareNum(name, answer)
		}
		val assignEquations = identifier ~ EQUAL() ~ count ~ SEMICOLON() ^^ {
			case IDENTIFIER(name) ~ _ ~ answer ~ _ => AssignNum(Variable(name), answer )
		}
		val ifcase = IF() ~ LEFTBRACKET() ~ condition ~ RIGHTBRACKET() ~ LEFTBRACE() ~ block ~ RIGHTBRACE() ^^ {
			case _ ~ _ ~ cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
		}
		val ifelse = ifcase ~ elseThen ^^ {
			case  ifThenExpr ~ elseThenExpr => Choice(List(ifThenExpr, elseThenExpr)) 
		}
		val printstr = PRINT() ~ LEFTBRACKET() ~ (/*addStr|*/strLiteral) ~ RIGHTBRACKET() ~ SEMICOLON() ^^ {
			//case _ ~ _ ~ addStrExpr ~ _ ~ _ => PrintExpr(addStrExpr.asInstanceOf[StrExpr])
			case _ ~ _ ~ STRLITERAL(lit) ~ _ ~ _ => PrintExpr(StrExpr(lit))
			//case _ ~ _ ~ addStrExpr ~ _ ~ _ ~ _ => PrintExpr(addStrExpr)
		} 
		val forcase = FOR() ~ LEFTBRACKET() ~ declareInt  ~ condition ~ SEMICOLON() ~ increment  ~ RIGHTBRACKET() ~ LEFTBRACE() ~ block ~ RIGHTBRACE() ^^ {
			case _ ~ _ ~ declareExpr ~ cond ~ _ ~ inc ~ _ ~ _  ~ block ~ _ => ForDo(declareExpr, cond, inc, block)
		}
		val printInt = PRINT() ~ LEFTBRACKET() ~ numLiteral ~ RIGHTBRACKET() ~ SEMICOLON() ^^ {
			case _ ~ _ ~ NUMLITERAL(lit) ~ _ ~ _ => PrintNumExpr(NumExpr(lit))
			//case _ ~ _ ~ addStrExpr ~ _ ~ _ ~ _ => PrintExpr(addStrExpr)
		}
		val printVar = PRINT() ~ LEFTBRACKET() ~ identifier ~ RIGHTBRACKET() ~ SEMICOLON() ^^ {
			case _ ~ _ ~ IDENTIFIER(name) ~ _ ~ _ => PrintVar(Variable(name))
			//case _ ~ _ ~ addStrExpr ~ _ ~ _ ~ _ => PrintExpr(addStrExpr)
		}

		
		val switchcase = SWITCH() ~ LEFTBRACKET() ~ identifier ~ RIGHTBRACKET() ~ LEFTBRACE() ~ rep1(caseThen)~ RIGHTBRACE() ^^{
			case _ ~ _ ~ IDENTIFIER(name) ~ _ ~ _ ~ caseList ~ _ => {
				var cases = new ListBuffer[JavaAST]()
				for(c <- caseList){
					var number = c.value
					var content = c.block
					c match {
						case CaseThen(StrExpr(lit), block) => cases += IfThen(StrEquals(Variable(name), number), content)
						case CaseThen(NumExpr(lit), block) => cases += IfThen(NumEquals(Variable(name), number), content)
					}
					
					
				}
				SwitchExpr(cases)
			}
		}

		val whilecase = WHILE() ~ LEFTBRACKET() ~ condition ~ RIGHTBRACKET() ~ LEFTBRACE() ~ block ~ RIGHTBRACE() ^^ {
			case _ ~ _ ~ cond ~ _ ~ _ ~ block ~ _ => WhileDo(cond, block)
		}
		val assignStr = identifier ~ EQUAL() ~ strLiteral ~ SEMICOLON() ^^{
			case IDENTIFIER(name) ~ _ ~ STRLITERAL(value) ~ _ => AssignStr(Variable(name), StrExpr(value))
		}


		/*val assignInt = identifier ~ EQUAL() ~ intLiteral ~ SEMICOLON() ^^{
			case IDENTIFIER(name) ~ _ ~ INTLITERAL(value) ~ _ => AssignNum(Variable(name), IntExpr(value))
		}*/
		
		/*val assignDouble = identifier ~ EQUAL() ~ doubleLiteral ~ SEMICOLON() ^^{
			case IDENTIFIER(name) ~ _ ~ DOUBLELITERAL(value) ~ _ => AssignNum(Variable(name), DoubleExpr(value))
		}*/
		
		declareStr| declareInt| declareDouble| assignEquations| assignStr| switchcase| ifelse| ifcase| whilecase| forcase | printstr| printInt| printVar /*assignDouble| assignInt*/ 
	 	//val ifcase = IF() ^^ (_=>Exit)
	 	//ifcase 
	}


	/*def ifThen: Parser[IfThen] = positioned {
	    (condition ~  ~ INDENT() ~ block ~ DEDENT()) ^^ {
	      case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
	  	}
	}*/

	/*def calculate: Parser[Arithmetic] = positioned {


		//rep1(count) ^^ {case cntList => cntList reduceRight SeqCount}
	}*/

	/*def combineStr: Parser[Arithmetic] = positioned{
		rep1(addStr) ^^ {case addList => stmtList reduceRight AndThen}
	}
*/


	def addStr: Parser[Arithmetic] = {
		(strLiteral|identifier) ~ ADD() ~ (strLiteral|identifier) ^^ {
			case STRLITERAL(lit1) ~ _ ~ STRLITERAL(lit2) => AddStrExpr(StrExpr(lit1), StrExpr(lit2))
			case IDENTIFIER(name) ~ _ ~ STRLITERAL(lit2) => AddStrExpr(Variable(name), StrExpr(lit2))
			case STRLITERAL(lit1) ~ _ ~ IDENTIFIER(name)  => AddStrExpr(StrExpr(lit1), Variable(name))
		}
	}

	def caseThen: Parser[CaseThen] = {
		CASE() ~ (numLiteral| strLiteral) ~ COLON() ~ block ~ BREAK() ~ SEMICOLON() ^^ {
			case _ ~ NUMLITERAL(lit) ~ _ ~ block ~ _ ~ _  => CaseThen(NumExpr(lit), block)
			case _ ~ STRLITERAL(lit) ~ _ ~ block ~ _ ~ _ => CaseThen(StrExpr(lit), block)
		}
	}

	def increment: Parser[AssignNum] = positioned {
		identifier ~ INC() ^^ {
			case IDENTIFIER(name) ~ _ => AssignNum(Variable(name), AddExpr(Variable(name), NumExpr(1)))
		}
	}

	def elseThen: Parser[ElseThen] = positioned{
		ELSE() ~ LEFTBRACE() ~ block ~ RIGHTBRACE() ^^ {
			case _ ~ _ ~ block ~ _ => ElseThen(block) 
		}
	}
	




	def count: Parser[Arithmetic] = positioned{
		/*val varAddInt = identifier ~ ADD() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ _ ~ INTLITERAL(lit) => AddExpr(Variable(name), IntExpr(lit)) 
		}
		val varAddDouble = identifier ~ ADD() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ _ ~ DOUBLELITERAL(lit) => AddExpr(Variable(name), DoubleExpr(lit)) 
		}*/
		/*val intAddInt = intLiteral ~ ADD() ~ intLiteral ^^ {
			case  INTLITERAL(left) ~ _ ~ INTLITERAL(right) => AddExpr(IntExpr(left), IntExpr(right)) 
		}
		val seqAddInt = calculate ~ ADD() ~ intLiteral ^^ {
			case answer ~ _ ~ INTLITERAL(right) => AddExpr(answer, IntExpr(right))
		}*/
		/*varAddInt| varAddDouble| seqAddInt|intAddInt*/
		//val factor = doubleLiteral | LEFTBRACKET() ~> expr ~ 
		//val plus: Parser[Arithmetic=>Arithmetic] =  rep(ADD() ~ doubleLiteral) ^^ { case _ ~ number}

		//def term: Parser[Arithmetic] = 

		def factor: Parser[Arithmetic] = matchNum | LEFTBRACKET() ~> equations <~ RIGHTBRACKET() 

		def matchNum: Parser[Arithmetic] = (numLiteral|identifier) ^^ {
			case NUMLITERAL(num) => NumExpr(num)
			case IDENTIFIER(name) => Variable(name)
		}

		def term: Parser[Arithmetic] = factor ~ rep(times| divide) ^^ {
			case numExpr ~ eqlist => {
				if (eqlist.length > 0){
					eqlist.foldLeft(numExpr.asInstanceOf[Arithmetic]){
						(acc, f) => f(acc)
					}
				}
				else{
					numExpr
				}
			}
		}

		def times: Parser[Arithmetic=>Arithmetic] = MUL() ~ factor ^^ {
			case _ ~ numExpr  =>  MulExpr(_, numExpr)
		}

		def divide: Parser[Arithmetic=>Arithmetic] = DIV() ~ factor ^^ {
			case _ ~numExpr => DivExpr(_, numExpr)
		}

		def plus: Parser[Arithmetic=>Arithmetic] = ADD()~ term ^^{
			case _ ~ numExpr => AddExpr(_, numExpr)
		}


		def minus: Parser[Arithmetic=>Arithmetic] = SUB()~ term ^^{
			case _ ~ numExpr => SubExpr(_, numExpr)
		}



		def equations: Parser[Arithmetic] = term ~ rep(plus|minus) ^^ {
			case numExpr ~ eqlist => {
				if (eqlist.length > 0){
					eqlist.foldLeft(numExpr.asInstanceOf[Arithmetic]){
						(acc, f) => f(acc)
					}
				}
				else{
					numExpr
				}
				
				//(DoubleExpr(num)/:eqlist)((DoubleExpr(num), f)=> f(DoubleExpr(num)))
			}
		}
		equations
	}

	

	def condition: Parser[Condition] = positioned {
		val strEquals = (strLiteral|identifier) ~ EQUALS() ~ (strLiteral| identifier) ^^ { 
			case IDENTIFIER(name) ~ eq ~ STRLITERAL(lit) => StrEquals(Variable(name), StrExpr(lit)) 
			case STRLITERAL(l1) ~ eq ~ STRLITERAL(l2) => StrEquals(StrExpr(l1), StrExpr(l2))
			case STRLITERAL(lit) ~ eq ~ IDENTIFIER(name) => StrEquals( StrExpr(lit), Variable(name)) 
		}

		val varEquals = identifier ~ EQUALS() ~ identifier ^^ {
			case IDENTIFIER(n1) ~ eq ~ IDENTIFIER(n2) => VarEquals(Variable(n1), Variable(n2))
		}
		
		/*val intEquals = identifier ~ EQUALS() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ INTLITERAL(lit) => NumEquals(Variable(name), IntExpr(lit))
		}
		val doubleEquals = identifier ~ EQUALS() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ DOUBLELITERAL(lit) => NumEquals(Variable(name), DoubleExpr(lit))
		}*/
		val numEquals = count ~ EQUALS() ~ count ^^ {
			case numExpr1 ~ eq ~ numExpr2 => NumEquals(numExpr1, numExpr2)
			
		}
		/*val intGreater = identifier ~ GREATERTHAN() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ INTLITERAL(lit) => NumGreater(Variable(name), IntExpr(lit))
		}
		val doubleGreater = identifier ~ GREATERTHAN() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ DOUBLELITERAL(lit) => NumGreater(Variable(name), DoubleExpr(lit))
		}*/
		val numGreater = count ~ GREATERTHAN() ~ count ^^ {
			case numExpr1 ~ eq ~ numExpr2 => NumGreater(numExpr1, numExpr2)
		}
		/*val intGreaterEquals = identifier ~ GREATEREQUALS() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ INTLITERAL(lit) => NumGreaterEqual(Variable(name), IntExpr(lit))
		}
		val doubleGreaterEquals = identifier ~ GREATEREQUALS() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ DOUBLELITERAL(lit) => NumGreaterEqual(Variable(name), DoubleExpr(lit))
		}*/
		val numGreaterEquals = count ~ GREATEREQUALS() ~ count ^^ {
			case numExpr1 ~ eq ~ numExpr2 => NumGreaterEqual(numExpr1, numExpr2)
		}
		/*val intSmaller = identifier ~ SMALLERTHAN() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ INTLITERAL(lit) => NumSmaller(Variable(name), IntExpr(lit))
		}
		val doubleSmaller = identifier ~ SMALLERTHAN() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ DOUBLELITERAL(lit) => NumSmaller(Variable(name), DoubleExpr(lit))
		}*/
		val numSmaller = count ~ SMALLERTHAN() ~ count ^^ {
			case numExpr1 ~ eq ~ numExpr2 => NumSmaller(numExpr1, numExpr2)
		}
		/*val intSmallerEquals = identifier ~ SMALLEREQUALS() ~ intLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ INTLITERAL(lit) => NumSmallerEqual(Variable(name), IntExpr(lit))
		}
		val doubleSmallerEquals = identifier ~ SMALLEREQUALS() ~ doubleLiteral ^^ {
			case IDENTIFIER(name) ~ eq ~ DOUBLELITERAL(lit) => NumSmallerEqual(Variable(name), DoubleExpr(lit))
		}*/
		val numSmallerEquals = count ~ SMALLEREQUALS() ~ count ^^ {
			case numExpr1 ~ eq ~ numExpr2 => NumSmallerEqual(numExpr1, numExpr2)
		}
		varEquals| strEquals | numEquals| numGreater| numGreaterEquals| numSmaller| numSmallerEquals /*intEquals|  doubleEquals| intGreater| doubleGreater| intSmaller| doubleSmaller|
		intGreaterEquals| doubleGreaterEquals| intSmallerEquals| doubleSmallerEquals*/
	}



	//private def counter: Parser[]

	private def identifier: Parser[IDENTIFIER] = positioned {
    	accept("identifier", { case id @ IDENTIFIER(name) => id })
  	}


  	private def numLiteral: Parser[NUMLITERAL] = positioned {
  		accept("num literal", {case lit @ NUMLITERAL(name)=> lit})
  	}

  	/*private def intLiteral: Parser[INTLITERAL] = positioned {
  		accept("int literal", {case lit @ INTLITERAL(name) => lit})
  	}

  	private def doubleLiteral: Parser[DOUBLELITERAL] = positioned {
  		accept("double literal", {case lit @ DOUBLELITERAL(name) => lit})
  	}
*/
	private def strLiteral: Parser[STRLITERAL] = positioned {
	    accept("string literal", { case lit @ STRLITERAL(name) => lit })
	}


}




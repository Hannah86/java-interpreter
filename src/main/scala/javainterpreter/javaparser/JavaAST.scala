package javainterpreter.javaparser

import scala.util.parsing.input.Positional

sealed trait JavaAST extends Positional

case class DeclareStr(variable: String, value: StrExpr) extends JavaAST
case class DeclareNum(variable: String, value: Arithmetic) extends JavaAST
//case class DeclareInt(variable: String, value: Arithmetic) extends JavaAST
//case class DeclareDouble(variable: String, value: Arithmetic) extends JavaAST
case class AssignNum(variable: Variable, value: Arithmetic) extends JavaAST 
//case class AssignInt(variable: Variable, value: Arithmetic) extends JavaAST
//case class AssignDouble(variable: Variable, value: Arithmetic) extends JavaAST
case class AssignStr(variable: Variable, value: StrExpr) extends JavaAST
case class AndThen(step1: JavaAST, step2: JavaAST) extends JavaAST
case class IfThen(predicate: Condition, thenBlock: JavaAST) extends JavaAST
case class WhileDo(predicate: Condition, thenBlock: JavaAST) extends JavaAST
case class ForDo(declare: JavaAST, predicate: Condition, increment:JavaAST, thenBlock:JavaAST) extends JavaAST
case class Choice(alternatives: Seq[JavaAST]) extends JavaAST
case class PrintExpr(message: StrExpr) extends JavaAST
case class PrintNumExpr(message: NumExpr) extends JavaAST
case class PrintVar(message: Variable) extends JavaAST
case class SwitchExpr(alternatives: Seq[JavaAST]) extends JavaAST


//sealed trait ConditionThen extends Positional { def thenBlock: JavaAST }
//case class IfDo(predicate: Condition, thenBlock: JavaAST) extends ConditionThen
case class ElseThen(thenBlock: JavaAST) extends JavaAST
case class CaseThen(pattern: Arithmetic,thenBlock: JavaAST) extends JavaAST{
	var value: Arithmetic = pattern
	var block: JavaAST = thenBlock
}



// Similar to the Boolexpr
sealed trait Condition extends Positional
case class StrEquals(factName: Arithmetic, factValue: Arithmetic) extends Condition
case class VarEquals(factName: Variable, factValue: Variable) extends Condition

case class NumEquals(factName: Arithmetic, factValue: Arithmetic) extends Condition
case class NumGreater(factName: Arithmetic, factValue: Arithmetic) extends Condition
case class NumSmaller(factName: Arithmetic, factValue: Arithmetic) extends Condition
case class NumGreaterEqual(factName: Arithmetic, factValue: Arithmetic) extends Condition
case class NumSmallerEqual(factName: Arithmetic, factValue: Arithmetic) extends Condition

/*case class IntEquals(factName: String, factValue: Int) extends Condition
case class IntGreater(factName: String, factValue: Int) extends Condition 
case class IntSmaller(factName: String, factValue: Int) extends Condition 
case class IntGreaterEqual(factName: String, factValue: Int) extends Condition 
case class IntSmallerEqual(factName: String, factValue: Int) extends Condition 

case class DoubleEquals(factName: String, factValue: Double) extends Condition
case class DoubleGreater(factName: String, factValue: Double) extends Condition
case class DoubleSmaller(factName: String, factValue: Double) extends Condition
case class DoubleGreaterEqual(factName: String, factValue: Double) extends Condition 
case class DoubleSmallerEqual(factName: String, factValue: Double) extends Condition */


sealed trait Arithmetic extends Positional
case class Variable(factName: String) extends Arithmetic {
	var name: String = factName
}
//case class IntExpr(value: Int) extends Arithmetic
//case class DoubleExpr(value: Double) extends Arithmetic
case class NumExpr(value: Double) extends Arithmetic{
	var name: Double = value
}

case class AddExpr(left: Arithmetic, right: Arithmetic) extends Arithmetic
case class SubExpr(left: Arithmetic, right: Arithmetic) extends Arithmetic
case class MulExpr(left: Arithmetic, right: Arithmetic) extends Arithmetic
case class DivExpr(left: Arithmetic, right: Arithmetic) extends Arithmetic


case class StrExpr(value: String) extends Arithmetic{
	var name:String = value
}
case class AddStrExpr(left:Arithmetic, right:Arithmetic) extends Arithmetic

/*sealed trait CharClass extends Positional
case class StrExpr(value: String) extends CharClass
case class StrVariable(name: String) extends CharClass 
case class AddStrExpr(left:CharClass, right:CharClass) extends CharClass

*/








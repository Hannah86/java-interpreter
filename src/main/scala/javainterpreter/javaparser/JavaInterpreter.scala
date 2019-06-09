package javainterpreter.javaparser

import scala.util.parsing.input.Positional
import scala.collection.mutable.Map
import scala.collection.mutable.ListMap
import scala.collection.mutable.HashMap


object JavaInterpreter{

class Env(env1:NumEnv, env2:StrEnv) {
	val number = env1
	val string = env2
}
type NumEnv = HashMap[String, Double]
type StrEnv = HashMap[String, String]

def evalArithmetic (exp: Arithmetic, store: Env): Double = exp match{
	case NumExpr(value: Double) => value
    case Variable(factName: String) => store.number(factName)
    case AddExpr(left: Arithmetic, right: Arithmetic) => 
    	evalArithmetic(left, store) + evalArithmetic(right, store)
    case SubExpr(left: Arithmetic, right: Arithmetic) => 
    	evalArithmetic(left, store) - evalArithmetic(right, store)
    case MulExpr(left: Arithmetic, right: Arithmetic) => 
    	evalArithmetic(left, store) * evalArithmetic(right, store)
    case DivExpr(left: Arithmetic, right: Arithmetic) =>
    	evalArithmetic(left, store) / evalArithmetic(right, store)
}

def evalArithmeticStr(exp: Arithmetic, store: Env): String = exp match{
	case StrExpr(value: String) => value
	case Variable(factName: String) => store.string(factName)
	case AddStrExpr(left:Arithmetic, right:Arithmetic) => 
		evalArithmeticStr(left, store) + evalArithmeticStr(right, store)
}

def evalCondition(exp: Condition, store: Env): Boolean = exp match{
	case StrEquals(factName: Arithmetic, factValue: Arithmetic) => 
		evalArithmeticStr(factName, store) == evalArithmeticStr(factValue, store)
	case VarEquals(factName: Variable, factValue: Variable) => 
		if ((store.number).contains(factName.name) && (store.number).contains(factValue.name))
			store.number(factName.name) == store.number(factName.name)
		else
			store.string(factName.name) == store.string(factValue.name)
	case NumEquals(factName: Arithmetic, factValue: Arithmetic) => 
		evalArithmetic(factName, store) == evalArithmetic(factValue, store)
    case NumGreater(factName: Arithmetic, factValue: Arithmetic) =>
    	evalArithmetic(factName, store) > evalArithmetic(factValue, store)
    case NumSmaller(factName: Arithmetic, factValue: Arithmetic) =>
    	evalArithmetic(factName, store) < evalArithmetic(factValue, store)
    case NumGreaterEqual(factName: Arithmetic, factValue: Arithmetic) =>
    	evalArithmetic(factName, store) >= evalArithmetic(factValue, store)
    case NumSmallerEqual(factName: Arithmetic, factValue: Arithmetic) =>
    	evalArithmetic(factName, store) <= evalArithmetic(factValue, store)
}

def evalJavaAST(command: JavaAST, store: Env): Env = command match{
	case DeclareNum(variable: String, value: Arithmetic) => 
		store.number += (variable -> evalArithmetic(value, store));store		
	case AssignNum(variable: Variable, value: Arithmetic) =>
        store.number += (variable.name -> evalArithmetic(value, store));store
    case DeclareStr(variable: String, value: StrExpr) =>
    	store.string += (variable -> evalArithmeticStr(value, store));store
    case AssignStr(variable: Variable, value: StrExpr) =>
    	store.string += (variable.name -> evalArithmeticStr(value, store));store
	case AndThen(step1: JavaAST, step2: JavaAST) => 
		evalJavaAST(step2, evalJavaAST(step1, store)); store
	case IfThen(predicate: Condition, thenBlock: JavaAST) =>
		if (evalCondition(predicate, store)) return evalJavaAST(thenBlock, store) else store
	case WhileDo(predicate: Condition, thenBlock: JavaAST) =>
		if (evalCondition(predicate, store))
			return evalJavaAST(AndThen(thenBlock, WhileDo(predicate, thenBlock)), store)
		else 
			return store
	case Choice(alternatives: Seq[JavaAST]) => alternatives(0) match {
		case IfThen(predicate: Condition, thenBlock: JavaAST) => {
			if (evalCondition(predicate, store)){
				return evalJavaAST(thenBlock, store)
			}
			else{
				evalJavaAST(alternatives(1), store)
			} 
		} 
	}
	case SwitchExpr(alternatives: Seq[JavaAST]) => {
		for (choice <- alternatives){
			choice match {
				case IfThen(predicate: Condition, thenBlock: JavaAST)  => {
					if (evalCondition(predicate, store)){
						return evalJavaAST(thenBlock, store)
					}
				}
			}
		}
		store
	}  	
	case ElseThen(thenBlock: JavaAST) => evalJavaAST(thenBlock, store)
	case PrintExpr(message: StrExpr) => 
		println(message.name); store	
	case PrintNumExpr(message: NumExpr) =>
		println(message.name);store
	case PrintVar(message: Variable) =>
		if ((store.number).contains(message.name)){
			println(store.number(message.name))
		}else{
			println(store.string(message.name))
		};
		store
	case ForDo(declare: JavaAST, predicate: Condition, increment: JavaAST, thenBlock: JavaAST) =>
		evalJavaAST(declare, store);
		while (evalCondition(predicate, store)){
		    evalJavaAST(thenBlock, store);
		    evalJavaAST(increment, store);
		}
		return store
		
		/*if (evalJavaAST(alternatives(0), store).number == store.number){
		    return store
		}
		else{
			evalJavaAST(alternatives(1), store)
		}*/	
			
}
}





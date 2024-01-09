/**
 * A simple arithmetic expression interpreter in Scala.
 *
 * This interpreter supports basic arithmetic operations (addition, subtraction, multiplication),
 * conditional expressions, and function definitions/applications. It utilizes a sealed trait
 * 'ArithExpr' to represent different kinds of expressions, including numbers, binary operations,
 * conditionals, and function-related constructs.
 *
 * Key Functionalities:
 * - Interpretation: The 'interpret' function evaluates expressions in the context of variable and
 *   function definition environments.
 * - Desugaring: The 'desugar' function simplifies certain expressions, such as converting subtraction
 *   to addition with negative numbers.
 * - Substitution: The 'substitute' function handles parameter replacement in function bodies, essential
 *   for function application.
 *
 * Example Usage:
 * To evaluate an expression, first, define it using the provided case classes, then call the
 * 'interpret' function with the expression and initial environments.
 *
 * Example:
 * val expr = Plus(Num(3), Mult(Num(4), Num(5)))
 * val result = interpret(expr, Map(), Map())
 *
 * Note: This interpreter throws RuntimeException for undefined variables or functions.
 */

sealed trait ArithExpr
case class Num(value: Int) extends ArithExpr
case class Plus(left: ArithExpr, right: ArithExpr) extends ArithExpr
case class Sub(left: ArithExpr, right: ArithExpr) extends ArithExpr
case class Mult(left: ArithExpr, right: ArithExpr) extends ArithExpr
case class IfThenElse(condition: ArithExpr, left: ArithExpr, right: ArithExpr) extends ArithExpr
case class Variable(name: String) extends ArithExpr
case class FunctionDef(name: String, params: List[Identifier], body: ArithExpr) extends ArithExpr
case class FunctionApp(name: String, args: List[ArithExpr]) extends ArithExpr

type FunctionDefEnv = Map[String, FunctionDef]
type VariableEnv = Map[String, Int]

var variableEnv: VariableEnv = Map()
var functionDefEnv: FunctionDefEnv = Map()

def interpret(expr: ArithExpr, variableEnv: variableEnv, functionDefEnv: functionDefEnv): Int = expr match {
    case Num(value) => value
    case Plus(left, right) => interpret(left, variableEnv, functionDefEnv) + interpret(right, variableEnv, functionDefEnv)
    case Mult(left, right) => interpret(left, variableEnv, functionDefEnv) * interpret(right, variableEnv, functionDefEnv)
    case IfThenElse(condition, left, right) => if (interpret(condition, variableEnv, functionDefEnv) != 0) interpret(left, variableEnv, functionDefEnv) else interpret(right, variableEnv, functionDefEnv)
    case Variable(name) => VariableEnv.getOrElse(name, throw new RuntimeException("Variable not defined: " + name))
    case FunctionApp(name, args) => 
        FunctionDefEnv.get(name) match {
            case Some(FunctionDef(_, params, body)) =>
                val argsValues = args.map(interpret, variableEnv, functionDefEnv)
                val newVariableEnv = params.zip(argsValues).toMap ++ variableEnv
                interpret(body, newVariableEnv, functionDefEnv)
            case None => throw new RuntimeException("Function not defined: " + name)
        }
}

def desugar(expr: ArithExpr): ArithExpr = expr match {
    case Num(_) => expr
    case Variable(_) => expr
    case Sub(left, right) => Plus(left, Mult(Num(-1), right))
    case Plus(left, right) => Plus(desugar(left), desugar(right))
    case Mult(left, right) => Mult(desugar(left), desugar(right))
    case IfThenElse(condition, left, right) => IfThenElse(desugar(condition), desugar(left), desugar(right))
}

def substitute(expr: ArithExpr, paramNames: List[String], paramValues: List[Int]): ArithExpr = {
    case Num(value) => Num(value)
    case Plus(left, right) => Plus(substitute(left, paramNames, paramValues), substitute(right, paramNames, paramValues))
    case Mult(left, right) => Mult(substitute(left, paramNames, paramValues), substitute(right, paramNames, paramValues))
    case IfThenElse(condition, left, right) => IfThenElse(substitute(condition, paramNames, paramValues), substitute(left, paramNames, paramValues), substitute(right, paramNames, paramValues))
    case Variable(name) => 
        paramNames.indexOf(name) match {
            case -1 => throw new IllegalArgumentException("Unbound identifier: " + name)
            case index => Num(paramValues(index))
        }
}

object ArithExprInterpreter {
    def runInterpreter(): Unit = {
        val expr = Plus(Num(3), Mult(Num(4), Num(5)))
        print(interpret(desugar(expr)))
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        ArithExprInterpreter.runInterpreter()
    }
}

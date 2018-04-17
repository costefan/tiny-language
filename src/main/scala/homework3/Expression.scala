package homework3

import java.beans.Expression
import java.security.KeyException

/**
 * Created by costefan on 09.04.18.
 */
sealed trait Expression {

//  def evalVariable(env: Map[String, Expression], key: String): Expression = {
//    val x: Option[Expression] = env get key
//    x match {
//      case Expression(n) =>
//    }
//  }
  def evalArg: Int = this match {
    case Number(n) => n
    case _ => throw new Exception("BlaBla")
  }

  def checkNumber(expr: Expression): Boolean = expr match {
    case Number(n) => true
    case _ => false
  }


  def eval(env: Map[String, Expression]): Expression = this match {
    case Number(n) => Number(n)
    case Var(a: String) => if (env contains a) env(a).eval(env) else throw new KeyException()
    case BooleanType(n) => BooleanType(n)
    case Sum(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) Number(lOp.evalArg + rOp.evalArg)
      else throw new Exception("Inconsistent sum types.")
    case Prod(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) Number(lOp.evalArg * rOp.evalArg)
      else throw new Exception("Inconsistent prod types.")
    case Less(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) BooleanType(lOp.evalArg < rOp.evalArg)
      else throw new Exception("We cannot compare not number types.")
//    case IfElse(lOp: Expression, rOp: Expression) =>
  }

  def showArg(argument: Expression, hPris: Boolean): String = {
    argument match {
      case Var(a) => a
      case Number(n) => n.toString
      case Sum(lOp, rOp) => if (hPris) "(" + lOp.show + " + " + rOp.show + " ) " else lOp.show + " + " + rOp.show
      case Prod(lOp, rOp) => lOp.show + " * " + rOp.show
    }
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Var(a) => a
    case Sum(lOp, rOp) => this.showArg(lOp, hPris = false) + " + " + this.showArg(rOp, hPris = false)
    case Prod(lOp, rOp) =>  this.showArg(lOp, hPris = true) + " * " + this.showArg(rOp, hPris = true)
  }

  def isReduciable: Boolean = {
    this match {
      case Number(n) => false
      case _ => true
    }
  }

  override def toString: String = this.show
}

sealed trait ReturnType

case class Number(n: Int) extends Expression
case class BooleanType(boolVal: Boolean) extends Expression

case class Sum(lOp: Expression, rOp: Expression) extends Expression
case class Less(lop: Expression, rOp: Expression) extends Expression
case class Var(a: String) extends Expression
case class Prod(lOp: Expression, rOp: Expression) extends Expression
case class IfElse(checker: Expression, lOp: Expression, rOp: Expression) extends Expression
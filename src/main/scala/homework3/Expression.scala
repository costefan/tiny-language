package homework3

import java.beans.Expression
import java.security.KeyException

/**
 * Created by costefan on 09.04.18.
 */
sealed trait Expression {

  def evalArg: Int = this match {
    case Number(n) => n
    case _ => throw new Exception("BlaBla")
  }

  def checkNumber(expr: Expression): Boolean = expr match {
    case Number(n) => true
    case _ => false
  }


  def eval(env: Map[String, Any]): Expression = this match {
    case Number(n) => Number(n)
    case BooleanType(n) => BooleanType(n)
    case Sum(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) Number(lOp.evalArg + rOp.evalArg)
      else throw new Exception("Inconsistent sum type")
    case Prod(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) Number(lOp.evalArg * rOp.evalArg)
      else throw new Exception("Inconsistent prod type")
    case Less(lOp: Expression, rOp: Expression) =>
      if (checkNumber(lOp) && checkNumber(rOp)) BooleanType(lOp.evalArg < rOp.evalArg)
      else throw new Exception("Inconsistent Less type")
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
    case BooleanType(n: Boolean) => n.toString
    case Sum(lOp, rOp) => this.showArg(lOp, hPris = false) + " + " + this.showArg(rOp, hPris = false)
    case Prod(lOp, rOp) =>  this.showArg(lOp, hPris = true) + " * " + this.showArg(rOp, hPris = true)
    case IfElse(checker, lOp, rOp) => "if " + this.showArg(checker, hPris = false) + "then" +
      this.showArg(lOp, hPris = false) + "else" + this.showArg(rOp, hPris = false)
  }

  def isReduciable: Boolean = {
    this match {
      case Number(n) => false
      case BooleanType(n) => false
      case _ => true
    }
  }

  override def toString: String = this.show
}

case class Number(n: Int) extends Expression
case class BooleanType(boolVal: Boolean) extends Expression

case class Sum(lOp: Expression, rOp: Expression) extends Expression
case class Less(lop: Expression, rOp: Expression) extends Expression
case class Var(a: String) extends Expression
case class Prod(lOp: Expression, rOp: Expression) extends Expression
case class IfElse(checker: Expression, lOp: Expression, rOp: Expression) extends Expression
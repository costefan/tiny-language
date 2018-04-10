package homework3

/**
 * Created by costefan on 09.04.18.
 */
sealed trait Expression {

  def evalArg(argument: Expression, hPris: Boolean): Int = {
    argument match {
      case Var(a) => 1
      case Number(n) => n
      case Sum(lOp, rOp) => lOp.eval + rOp.eval
      case Prod(lOp, rOp) => lOp.eval * rOp.eval
    }
  }

  def eval: Int = this match {
    case Number(n) => n
    case Sum(lOp, rOp) => lOp.eval +  rOp.eval
    case Prod(lOp, rOp) => lOp.eval * rOp.eval
    case Var(a) => 1
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
      case Var(a) => true
      case Sum(lOp, rOp) => true
      case Prod(lOp, rOp) =>  true
    }
  }

  override def toString: String = this.show
}

sealed trait ReturnType

case class Number(n: Int) extends Expression
case class BooleanType(boolVal: Boolean) extends Expression

case class Sum(lOp: Expression, rOp: Expression) extends Expression
case class Var(a: String) extends Expression
case class Prod(lOp: Expression, rOp: Expression) extends Expression
case class IfElse(checker: Expression, lOp: Expression, rOp: Expression) extends Expression
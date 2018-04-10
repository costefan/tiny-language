/**
  * Created by costefan on 09.04.18.
  */

package prehomework3

object PreAssignment {
  sealed trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(lOp, rOp) => lOp.eval +  rOp.eval
      case Prod(lOp, rOp) => lOp.eval * rOp.eval
    }

    def showArg(argument: Expr, hPris: Boolean): String = {
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
  }

  case class Number(n: Int) extends Expr
  case class Sum(lOp: Expr, rOp: Expr) extends Expr
  case class Var(a: String) extends Expr
  case class Prod(lOp: Expr, rOp: Expr) extends Expr

  def main(args: Array[String]): Unit = {
    println(new Sum(new Number(45), new Number(32)).eval)
    println(new Sum(new Number(45), new Number(32)).show)
    println(new Prod(new Sum(new Number(2), Var("x")), Var("y")).show)
    println(new Sum(new Prod(new Number(2), new Var("x")), Var("y")).show)
    println(new Prod(new Sum(new Number(3), new Number(2)), new Sum(new Number(55), new Number(66))).show)
  }
}
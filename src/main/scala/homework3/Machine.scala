/**
  * Created by costefan on 09.04.18.
  */
package homework3



final class Machine {

  def run(expr: Expression): Expression = {
    println(expr)

    if (expr.isReduciable)
      run(reductionStep(expr))
    else
      expr
  }

  def reductionStep(expr: Expression): Expression = {
    expr match {
      case Number(n) => Number(n)
      case Prod(lOp, rOp) => Number(reductionStep(lOp).eval * reductionStep(rOp).eval)
      case Sum(lOp, rOp) => Number(reductionStep(lOp).eval + reductionStep(rOp).eval)
    }
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    println(
      new Machine().run(
        Prod(Sum(Number(3), Number(2)),
             Sum(Number(2), Number(1)))
      )
    )
  }
}
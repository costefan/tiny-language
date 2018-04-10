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
      case Prod(lOp, rOp) =>
        if (lOp.isReduciable) Prod(reductionStep(lOp), rOp)
        else if (rOp.isReduciable) Prod(lOp, reductionStep(rOp))
        else Number(expr.eval)
      case Sum(lOp, rOp) =>
        if (lOp.isReduciable) Sum(reductionStep(lOp), rOp)
        else if (rOp.isReduciable) Sum(lOp, reductionStep(rOp))
        else Number(expr.eval)
    }
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    println(
      new Machine().run(
        Prod(Sum(Number(3), Number(2)),
          Number(5))
      )
    )
  }
}
/**
  * Created by costefan on 09.04.18.
  */
package homework3

import java.security.KeyException


final class Machine {

  def run(expr: Expression, env: Map[String, Expression]): Expression = {
    println(expr)

    if (expr.isReduciable)
      run(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expression, env: Map[String, Expression]): Expression = {
    expr match {
      case Prod(lOp, rOp) =>
        if (lOp.isReduciable) Prod(reductionStep(lOp, env), rOp)
        else if (rOp.isReduciable) Prod(lOp, reductionStep(rOp, env))
        else expr.eval(env)
      case Sum(lOp, rOp) =>
        if (lOp.isReduciable) Sum(reductionStep(lOp, env), rOp)
        else if (rOp.isReduciable) Sum(lOp, reductionStep(rOp, env))
        else expr.eval(env)
      case Var(a) => if (env contains a) env(a).eval(env) else throw new KeyException(
        "Key was not found in Map object")
    }
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    println(
      new Machine().run(
        Prod(Sum(Var("x"), Number(2)), Number(5)),
        Map("x" -> Number(3), "y" -> Number(30))
      )
    )
  }
}
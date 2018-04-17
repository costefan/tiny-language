import homework3._

final class Machine {

  def run(stat: Statement, env: Map[String, Any]): Map[String, Any] = {
    stat match {
      case DoNothing(expr: Expression) => reduce(expr, env)
      case Assign(variable: String, expr: Expression) =>
        env.updated(variable, reduce(expr, env))
      case If(expression: Expression, stat1: Statement, stat2: Statement) =>
        expression match {
          case BooleanType(n) => if (n) run(stat1, env) else run(stat2, env)
          case _ => env.updated("__errors", "Express should be Boolean")
            env
        }
      case While(expr: Expression, stat: Statement) => {
        val reduced = reduce(expr, env)
        reduced match {
          case BooleanType(n) => if (n) run(stat, env)
          case _ => env.updated("__error", "Error")
            env
        }
      }
//      case Seq(stats: Statement) =>
    }

    env
  }

  def reduce(expr: Expression, env: Map[String, Any]): Expression = {
    println(expr)
    try
      if (expr.isReduciable)
          reduce(reductionStep(expr, env), env)
      else
        expr
    catch {
      case exc: Exception =>
        println(exc.getMessage)
        expr
    }
  }

  def reductionStep(expr: Expression, env: Map[String, Any]): Expression = {
    expr match {

      case Prod(lOp, rOp) =>
        if (lOp.isReduciable) Prod(reductionStep(lOp, env), rOp)
        else if (rOp.isReduciable) Prod(lOp, reductionStep(rOp, env))
        else expr.eval(env)

      case Sum(lOp, rOp) =>
        if (lOp.isReduciable) Sum(reductionStep(lOp, env), rOp)
        else if (rOp.isReduciable) Sum(lOp, reductionStep(rOp, env))
        else expr.eval(env)

      case Var(a) => if (env contains a) {
        env(a) match {
          case x: Int => Number(x)
          case x: Boolean => BooleanType(x)
          case _ => throw new Exception("Key was not found in Map object")
        }
      } else throw new Exception("Key was not found in Map object")

      case Less(lOp, rOp) =>
        if (lOp.isReduciable) Less(reductionStep(lOp, env), rOp)
        else if (rOp.isReduciable) Less(lOp, reductionStep(rOp, env))
        else expr.eval(env)

      case IfElse(checker, lOp: Expression, rOp: Expression) =>
        checker match {
          case BooleanType(n) => if (n) reductionStep(lOp, env) else reductionStep(rOp, env)
          case _ => throw new Exception("Checker should be bool type")

        }
    }
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    val env = Map("x" -> 3, "y" -> 30)

    println(
      new Machine().reduce(
        Prod(Sum(Var("x"), Number(2)), Sum(Var("y"), Number(2))),
        env
      )
    )
//    var a = new Machine().reduce(
//        IfElse(BooleanType(true), Number(2), Number(3)),
//        env
//    )
  }
}
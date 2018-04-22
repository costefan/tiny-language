import homework3._

final class Machine {

  def run(stat: Statement, env: Map[String, Any]): Map[String, Any] = {
    try
      stat match {
        case DoNothing(expr: Expression) =>
          reduce(expr, env)
          env
        case Assign(variable: String, expr: Expression) =>
          env.updated(variable, reduce(expr, env))
        case If(expression: Expression, stat1: Statement, stat2: Statement) =>
          expression match {
            case BooleanType(n) => if (n) run(stat1, env) else run(stat2, env)
            case _ =>
              env.updated("__errors", "Express should be Boolean")
          }
        case While(expr: Expression, stat: Statement) => {
          val reduced = reduce(expr, env)
          reduced match {
            case BooleanType(n) =>
              var newEnv = env
              while(n) {
                println(newEnv)
                newEnv = run(stat, newEnv)
              }
              env
            case _ =>
              throw new Exception("Error, cannot reduce to Bool type")
          }
        }
      }
    catch {
      case exc: Exception =>
        val msg = exc.getMessage
        env.updated("__error", msg)
    }
  }

  def reduce(expr: Expression, env: Map[String, Any]): Expression = {
    println(expr)
    if (expr.isReduciable)
        reduce(reductionStep(expr, env), env)
    else
      expr
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
    println(
      new Machine().reduce(
        Less(
          Var("x"),
          Var("y")
        ), env
      )
    )
    val st = new Machine().run(
      Assign("x", Number(5)), env
    )
    println(st)
    println(
      new Machine().run(
        DoNothing(
        IfElse(
          BooleanType(true),
          Var("z"),
          Var("y")
        )
        ), env
      )
    )
    new Machine().run(
        If(BooleanType(false), Assign("x", Number(5)), Assign("x", Number(22))), env
    )
//    var a = new Machine().run(
//      While(
//        Less(Var("x"), Number(10)),
//        Assign("x", Sum(Var("x"), Number(1)))
//      ), env
//    )
//    println(a)


  }
}
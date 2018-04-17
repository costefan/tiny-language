import homework3.Expression

/**
  * Created by costefan on 17.04.18.
  */

sealed trait Statement {

}

case class DoNothing(expression: Expression) extends Statement
case class Assign(variable: String, expr: Expression) extends Statement
case class If(expression: Expression, stat1: Statement, stat2: Statement) extends Statement
case class While(expr: Expression, stat: Statement) extends Statement
case class Seq(stats: Statement*) extends Statement
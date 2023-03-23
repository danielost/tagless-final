package ua.danielost

object Solution {

  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean) extends Expr
    case class And(a: Expr, b: Expr) extends Expr
    case class Or(a: Expr, b: Expr) extends Expr
    case class Not(expr: Expr) extends Expr

    val exprTest: Expr = Or(And(B(true), B(false)), B(false))

    // Now what if we want to include ints?
    def eval(expr: Expr): Boolean = expr match {
      case B(b) => b
      case And(a, b) => eval(a) && eval(b)
      case Or(a, b) => eval(a) || eval(b)
      case Not(a) => !eval(a)
    }
  }

  def main(args: Array[String]) : Unit = {

  }

}

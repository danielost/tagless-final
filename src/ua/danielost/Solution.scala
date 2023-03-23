package ua.danielost

object Solution {

  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean) extends Expr
    case class And(a: Expr, b: Expr) extends Expr
    case class Or(a: Expr, b: Expr) extends Expr
    case class Not(expr: Expr) extends Expr

    val exprTest: Expr = Or(And(B(true), B(false)), B(false))

    def eval(expr: Expr): Boolean = expr match {
      case B(b) => b
      case And(a, b) => eval(a) && eval(b)
      case Or(a, b) => eval(a) || eval(b)
      case Not(a) => !eval(a)
    }

    case class I(int: Int) extends Expr
    case class Sum(a: Expr, b: Expr) extends Expr

    // Solution with tags (the bad one)
    object TagSolution {
      trait Expr {
        val tag: String
      }
      class BoolExpr extends Expr {
        override val tag: String = "bool"
      }
      class IntExpr extends Expr {
        override val tag: String = "int"
      }
      case class B(boolean: Boolean) extends BoolExpr
      case class And(a: Expr, b: Expr) extends BoolExpr
      case class Or(a: Expr, b: Expr) extends BoolExpr
      case class Not(expr: Expr) extends BoolExpr
      case class I(int: Int) extends IntExpr
      case class Sum(a: Expr, b: Expr) extends IntExpr

      def eval(expr: Expr): Any = expr match {
        case B(b) => b
        case And(a, b) =>
          if (a.tag != "bool" || b.tag != "bool")
            throw new IllegalArgumentException("Bad argument type")
          else
            eval(a).asInstanceOf[Boolean] && eval(b).asInstanceOf[Boolean]
//        same for the other cases
//        case Or(a, b) => eval(a) || eval(b)
//        case Not(a) => !eval(a)
      }
    }
  }

  def main(args: Array[String]) : Unit = {

  }

}

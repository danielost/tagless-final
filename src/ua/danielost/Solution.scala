package ua.danielost

object Solution {

  object ExpressionProblem {
//    trait Expr
//    case class B(boolean: Boolean) extends Expr
//    case class And(a: Expr, b: Expr) extends Expr
//    case class Or(a: Expr, b: Expr) extends Expr
//    case class Not(expr: Expr) extends Expr
//
//    val exprTest: Expr = Or(And(B(true), B(false)), B(false))
//
//    def eval(expr: Expr): Boolean = expr match {
//      case B(b) => b
//      case And(a, b) => eval(a) && eval(b)
//      case Or(a, b) => eval(a) || eval(b)
//      case Not(a) => !eval(a)
//    }
//
//    case class I(int: Int) extends Expr
//    case class Sum(a: Expr, b: Expr) extends Expr

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
      case class And(a: Expr, b: Expr) extends BoolExpr {
        assert(a.tag == "bool" || b.tag == "bool")
      }
      case class Or(a: Expr, b: Expr) extends BoolExpr
      case class Not(expr: Expr) extends BoolExpr
      case class I(int: Int) extends IntExpr
      case class Sum(a: Expr, b: Expr) extends IntExpr

      def eval(expr: Expr): Any = expr match {
        case B(b) => b
        case Or(a, b) => eval(a).asInstanceOf[Boolean] || eval(b).asInstanceOf[Boolean]
//      same for the other cases
      }
    }

    object TaglessInitial {
      trait Expr[T]
      case class B(boolean: Boolean) extends Expr[Boolean]
      case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
      case class Or(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
      case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
      case class I(int: Int) extends Expr[Int]
      case class Sum(a: Expr[Int], b: Expr[Int]) extends Expr[Int]

      def eval[T](expr: Expr[T]): T = expr match {
        case B(b) => b.asInstanceOf[T]
        case I(i) => i.asInstanceOf[T]
        case Or(a, b) => (eval(a) || eval(b)).asInstanceOf[T]
        case Sum(a, b) => (eval(a) + eval(b)).asInstanceOf[T]
        // same for the other cases
      }
    }

    def demoTaglessInitial(): Unit = {
      import TaglessInitial._
      println(eval(Or(B(true), B(true))))
      println(eval(Sum(I(15), I(-5))))
    }

    object TaglessFinal {
      trait Expr[T] {
        val value: T
      }

      def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
        override val value: Boolean = boolean
      }

      def i(int: Int): Expr[Int] = new Expr[Int] {
        override val value: Int = int
      }

      def or(a: Expr[Boolean], b: Expr[Boolean]) = new Expr[Boolean] {
        override val value: Boolean = a.value || b.value
      }

      def sum(a: Expr[Int], b: Expr[Int]) = new Expr[Int] {
        override val value: Int = a.value + b.value
      }

      def eval[T](expr: Expr[T]): T = expr.value
    }

    def demoFinalTagless(): Unit = {
      import TaglessFinal._
      println(eval(or(b(true), b(true))))
      println(eval(sum(i(15), i(-5))))
    }
  }

  def main(args: Array[String]) : Unit = {
    import ExpressionProblem._
    demoTaglessInitial()
    println("---------")
    demoFinalTagless()
  }
}

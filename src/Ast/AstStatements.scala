package Ast

trait AstStatement extends AstNode {
  def eval(env:AstEnv): AstEnv
}

case class AstAssignment(lhs: String, rhs: AstExpr) extends AstStatement {
  def eval(env: AstEnv): AstEnv = {
    val right = rhs.eval(env)
    env + (lhs -> right)
  }
  override def toString: String = s"let $lhs = $rhs"
}

case class AstPrint(expr: AstExpr) extends AstStatement {
  def eval(env: AstEnv): AstEnv = {
    println(s"${expr.eval(env)}")
    env
  }
  override def toString: String = s"print $expr"
}


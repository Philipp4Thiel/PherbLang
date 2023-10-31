import Ast.*
import TypeChecker.*
@main
def main(): Unit = {
  val prog = AstProgram(List(
    AstAssignment("add", AstExprLambda("y", AstExprLambda("z",AstExprBinOp(BinOp.Add, AstExprVar("y"), AstExprVar("z"))))),
    AstAssignment("a", AstExprBinOp(BinOp.Add, AstExprConst(1), AstExprApp(AstExprApp(AstExprVar("add"), AstExprConst(2)), AstExprConst(3)))),
  ))


  val typeCheckerRes = TypeChecker().check(prog)
  if typeCheckerRes.success then
    println("Type checking successful")
  else
    println("Type checking failed:")
    for err <- typeCheckerRes.getErrors do
      println(s"- $err")
  println()

  //prog.printAndEval()
  prog.eval()
}

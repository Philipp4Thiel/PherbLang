import Ast.*
import TypeChecker.*
@main
def main(): Unit = {
  val prog = AstProgram(List(
    // let apply = \f.\x.f x
    AstAssignment("apply", AstExprLambda("f",AstExprLambda("x", AstExprApp(AstExprVar("f"), AstExprVar("x")),Set("f")))),
    // let id = \x.x
    AstAssignment("id", AstExprLambda("x", AstExprVar("x"))),
    // let res = apply id 42
    // AstAssignment("res", AstExprApp(AstExprApp(AstExprVar("apply"), AstExprVar("id")), AstExprConst(42))),
    AstAssignment("res", AstExprApp(AstExprVar("apply"), AstExprConst(42))),
    // print res
    AstPrint(AstExprVar("res"))
  ))


  val typeCheckerRes = TypeChecker().check(prog)
  if typeCheckerRes.success then
    println("Type checking successful")
  else
    println("Type checking failed:")
    for err <- typeCheckerRes.getErrors do
      println(s"- $err")
  println()

  prog.printAndEval()
  //prog.eval()
}

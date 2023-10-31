package Ast

class AstProgram(val program: List[AstStatement]) {
  def eval(env:AstEnv = Map()) :AstEnv = {
    program.foldLeft(env)((acc, stmt) => stmt.eval(acc))
  }
  def printAndEval(env: AstEnv = Map()): AstEnv = {
    var env:AstEnv = Map()
    for (statement <- program) {
      print(statement)
      val isPrint = statement.isInstanceOf[AstPrint]
      if isPrint then
        print(" // ")
      env = statement.eval(env)
      if !isPrint then
        println()
    }
    env
  }

  override def toString = program.mkString("\n")
}



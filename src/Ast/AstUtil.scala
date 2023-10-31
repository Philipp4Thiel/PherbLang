package Ast

enum UnOp {
  case Neg
}

enum BinOp {
  case Add, Sub, Mul, Div, Eq, Neq, Lt, Le, Gt, Ge, And, Or
}

type AstEnv = Map[String, AstExpr]
type AstKnownUnknowns = Set[String]

class EvalException(msg: String) extends Exception(msg)
package TypeChecker

import Ast.*

class TypeChecker() {
  private var knownTypes = Map[AstExpr, PherbType]()
  private val constraints = TypeConstraints()
  private var counter = 0

  def check(program: AstProgram): TypeCheckerResult = {
    val res = TypeCheckerResult()
    for (stmt <- program.program) do
      stmt match {
        case AstAssignment(lhs, rhs) =>
          check(rhs) match {
            case Right(err) => res.addError(s"$err")
            case Left(t) =>
              if knownTypes.contains(AstExprVar(lhs)) && knownTypes(AstExprVar(lhs)) != t then
                constraints.addConstraint(Set(knownTypes(AstExprVar(lhs)), t))
              else
                knownTypes += (AstExprVar(lhs) -> t)
          }
        case AstPrint(expr) => check(expr) match {
          case Right(err) => res.addError(err)
          case Left(_) => ()
        }
      }
    constraints.solve() match {
      case Some(err) => res.addError(err)
      case None => ()
    }
    res
  }

  def check(expr: AstExpr): Either[PherbType, String] = {
    if knownTypes.contains(expr) then return Left(knownTypes(expr))
    val T: PherbType = expr match {
      case AstExprConst(_) => PherbType.Int
      case AstExprBinOp(_, op1, op2) => {
        check(op1) match {
          case Left(t) =>
            constraints.addConstraint(Set(t, PherbType.Int))
          case Right(err) => return Right(err)
        }
        check(op2) match {
          case Left(t) =>
            constraints.addConstraint(Set(t, PherbType.Int))
          case Right(err) => return Right(err)
        }
        PherbType.Int
      }
      case AstExprUnOp(_, op) => {
        check(op) match {
          case Left(t) =>
            constraints.addConstraint(Set(t, PherbType.Int))
            PherbType.Int
          case Right(err) => return Right(err)
        }
      }
      case AstExprLambda(arg_name, body, _) => {
        val arg_T = check(AstExprVar(arg_name)) match {
          case Left(t) => t
          case Right(err) => return Right(err)
        }
        val ret_T = check(body) match {
          case Left(t) => t
          case Right(err) => return Right(err)
        }
        PherbType.Func(arg_T, ret_T)
      }
      case AstExprApp(func, arg) => {
        val (func_arg_T, func_ret_T) = check(func) match {
          case Left(PherbType.Func(arg_T, ret_T)) => (arg_T, ret_T)
          case Left(PherbType.Unknown(x)) => {
            val arg_T = PherbType.Unknown(getId())
            val ret_T = PherbType.Unknown(getId())
            knownTypes += (func -> PherbType.Func(arg_T, ret_T))
            (arg_T, ret_T)
          }
          case Left(t) => return Right(s"trying to apply non-function $func of type $t")
          case Right(err) => return Right(err)
        }
        val given_arg_T = check(arg) match {
          case Left(t) => t
          case Right(err) => return Right(err)
        }
        constraints.addConstraint(Set(func_arg_T, given_arg_T))
        func_ret_T
      }
      case AstExprVar(v) => {
        val t = PherbType.Unknown(getId())
        t
      }
      case AstExprCond(cond, then_branch, else_branch) => {
        check(cond) match {
          case Left(PherbType.Int) => ()
          case Left(t) =>
            constraints.addConstraint(Set(t, PherbType.Int))
          case Right(err) => return Right(err)
        }
        val then_T = check(then_branch) match {
          case Left(t) => t
          case Right(err) => return Right(err)
        }
        val else_T = check(else_branch) match {
          case Left(t) => t
          case Right(err) => return Right(err)
        }
        constraints.addConstraint(Set(then_T, else_T))
        then_T
      }
    }
    knownTypes += (expr -> T)
    Left(T)
  }

  private def getId(): Int = {
    counter += 1
    counter
  }
}

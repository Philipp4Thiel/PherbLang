package Ast

trait AstExpr {
  def eval(env: AstEnv): AstExpr
}

case class AstExprConst(value: Int) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = AstExprConst(value)

  override def toString: String = value.toString
}

case class AstExprBinOp(op: BinOp, left: AstExpr, right: AstExpr) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    val l = left.eval(env)
    val r = right.eval(env)

    (l, r) match {
      case (AstExprConst(lv), AstExprConst(rv)) => op match
        case BinOp.Add => AstExprConst(lv + rv)
        case BinOp.Sub => AstExprConst(lv - rv)
        case BinOp.Mul => AstExprConst(lv * rv)
        case BinOp.Div => AstExprConst(lv / rv)
        case BinOp.Eq => AstExprConst(if lv == rv then 1 else 0)
        case BinOp.Ge => AstExprConst(if lv >= rv then 1 else 0)
        case BinOp.Gt => AstExprConst(if lv > rv then 1 else 0)
        case BinOp.Le => AstExprConst(if lv <= rv then 1 else 0)
        case BinOp.Lt => AstExprConst(if lv < rv then 1 else 0)
        case BinOp.Neq => AstExprConst(if lv != rv then 1 else 0)
        case BinOp.And => AstExprConst(if lv != 0 && rv != 0 then 1 else 0)
        case BinOp.Or => AstExprConst(if lv != 0 || rv != 0 then 1 else 0)
      case (l, r) => AstExprBinOp(op, l, r)
    }
  }

  override def toString: String = op match
    case BinOp.Add => s"($left + $right)"
    case BinOp.Sub => s"($left - $right)"
    case BinOp.Mul => s"($left * $right)"
    case BinOp.Div => s"($left / $right)"
    case BinOp.Eq => s"($left == $right)"
    case BinOp.Ge => s"($left >= $right)"
    case BinOp.Gt => s"($left > $right)"
    case BinOp.Le => s"($left <= $right)"
    case BinOp.Lt => s"($left < $right)"
    case BinOp.Neq => s"($left != $right)"
    case BinOp.And => s"($left && $right)"
    case BinOp.Or => s"($left || $right)"
}

case class AstExprUnOp(op: UnOp, node: AstExpr) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    val n = node.eval(env)

    n match {
      case AstExprConst(v) => op match
        case UnOp.Neg => AstExprConst(-v)
      case n => AstExprUnOp(op, n)
    }
  }

  override def toString: String = op match
    case UnOp.Neg => s"(-$node)"
}

case class AstExprVar(name: String) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    if (env.contains(name)) {
      env(name)
    } else {
      AstExprVar(name)
    }
  }

  override def toString: String = name
}

case class AstExprLambda(name: String, body: AstExpr, var envAtFirstEval: Option[AstEnv] = None) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    if (envAtFirstEval.isEmpty) {
      envAtFirstEval = Some(env)
    }
    AstExprLambda(name, body.eval(envAtFirstEval.get), envAtFirstEval)
  }

  override def toString: String = s"\\$name.$body"
}

case class AstExprApp(lam: AstExpr, arg: AstExpr) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    val l = lam.eval(env).asInstanceOf[AstExprLambda]
    val a = arg.eval(env)
    val e = l.envAtFirstEval.get.foldLeft(env)((acc, kv) => acc + kv)
    val res = l.body.eval(e + (l.name -> a))
    res
  }

  override def toString: String = s"($lam $arg)"
}

case class AstExprCond(condValue: AstExpr, nonZeroValue: AstExpr, zeroValue: AstExpr = AstExprConst(0)) extends AstExpr {
  override def eval(env: AstEnv): AstExpr = {
    val c = condValue.eval(env)

    c match {
      case AstExprConst(0) => zeroValue.eval(env)
      case AstExprConst(_) => nonZeroValue.eval(env)
      case c => AstExprCond(c, nonZeroValue, zeroValue)
    }
  }

  override def toString: String = s"if $condValue then $nonZeroValue else $zeroValue end"
}

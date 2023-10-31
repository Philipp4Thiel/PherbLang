package TypeChecker

type Mapping = Map[PherbType, PherbType]

class TypeConstraints {
  var constraints: Set[Set[PherbType]] = Set()
  private var mapping: Mapping = Map()

  def addConstraint(set: Set[PherbType]): Unit = {
    var newConstraints: Set[Set[PherbType]] = Set()
    var containsNewVals: Set[PherbType] = Set()
    for (constraint <- constraints) {
      if (constraint.intersect(set).isEmpty) {
        newConstraints += constraint
      } else {
        containsNewVals = containsNewVals.union(constraint)
      }
    }
    newConstraints += set.union(containsNewVals)
    constraints = newConstraints
  }

  def get(pherbType: PherbType): PherbType = {
    var currentType = pherbType
    var nextType = mapping.getOrElse(currentType, currentType)
    while (currentType != nextType) {
      currentType = nextType
      nextType = mapping.getOrElse(currentType, currentType)
    }
    currentType
  }

  def solve(): Option[String] = {
    var curMapping = generateNewMapping() match {
      case Left(m) => m
      case Right(s) => return Some(s)
    }
    while (!equals(curMapping, mapping)) {
      mapping = curMapping
      curMapping = generateNewMapping() match {
        case Left(m) => m
        case Right(s) => return Some(s)
      }
    }
    None
  }

  private def equals(m1: Mapping, m2: Mapping): Boolean = {
    for ((k, v) <- m1) {
      if (!m2.get(k).contains(v)) {
        return false
      }
    }
    true
  }

  private def generateNewMapping(): Either[Mapping, String] = {
    var newMapping: Mapping = Map()
    for set <- constraints if set.size > 1 do {
      var unknown = set.filter(_ match {
        case PherbType.Unknown(_) => true
        case _ => false
      })
      val known = set.filter(_ match {
        case PherbType.Unknown(_) => false
        case _ => true
      })
      val merged = if (known.isEmpty) {
        val tmp: PherbType = unknown.head
        unknown = unknown - tmp
        tmp
      } else {
        merge(known) match {
          case Left(t) => t
          case Right(s) => return Right(s)
        }
      }

      for (u <- unknown.union(known)) {
        newMapping += (u -> merged)
      }
    }
    Left(newMapping)
  }

  private def merge(set: Set[PherbType]): Either[PherbType,String] = {
    if set.size == 1 then
      Left(set.head)
    else
      val t1 = set.head
      val t2 = merge(set.tail) match
        case Left(t) => t
        case Right(s) => return Right(s)
      merge(t1, t2)
  }

  private def merge(t1: PherbType, t2: PherbType): Either[PherbType,String] = {
    (get(t1), get(t2)) match {
      case (PherbType.Unknown(_), _) => Left(t2)
      case (_, PherbType.Unknown(_)) => Left(t1)
      case (PherbType.Int, PherbType.Int) => Left(PherbType.Int)
      case (PherbType.Func(t1, t2), PherbType.Func(t3, t4)) =>
        merge(t1, t3) match {
          case Left(t_arg) => merge(t2, t4) match {
            case Left(t_ret) => Left(PherbType.Func(t_arg, t_ret))
            case Right(s) => Right(s)
          }
          case Right(s) => Right(s)
        }
      case _ => Right(s"Cannot merge $t1 and $t2")
    }
  }
}


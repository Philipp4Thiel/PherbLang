package TypeChecker

import scala.collection.mutable

class TypeCheckerResult() {
  private val errors = mutable.ListBuffer[String]()

  def success = errors.isEmpty

  def addError(err: String) = {
    errors += err
  }

  def getErrors = errors
}

enum PherbType {
  case Int
  case Func(argType: PherbType, returnType: PherbType)
  case Unknown(id: Int)
}


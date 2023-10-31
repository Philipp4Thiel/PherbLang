package Parser


enum Tokens {
  case NewLine
  case BracketOpen
  case BracketClose 
  case Identifier(identifier: String)
  case Number(number: Int)
  case Operator(operator: Operators) 
  case Backslash 
  case Point 
  case Let
  case Assign
  case Print
}

enum Operators {
  case Plus
  case Minus
  case Multiply
  case Divide
  case Gt
  case Lt
  case Ge
  case Le
  case Eq
  case Ne
  case And
  case Or
}
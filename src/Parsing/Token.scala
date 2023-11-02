package Parsing

import Parsing.Token.*

enum Token {
  case EOF
  case NewLine
  case Colon
  case SemiColon
  case Assign
  case BracketOpen
  case BracketClose
  case Comma
  case Arrow
  case QuestionMark

  // keywords:
  case Print
  case Let
  case Int
  case Lambda

  // operators:
  case Plus
  case Minus
  case Multiply
  case Divide
  case Equals
  case NotEquals
  case GreaterThan
  case LessThan
  case GreaterThanOrEqual
  case LessThanOrEqual
  case And
  case Or
  case Not

  case Literal(value: Int)
  case Identifier(value: String)

  case Unknown
}

def isBinOperator(token: Token) = token match {
  case Plus | Minus | Multiply | Divide | Equals
       | NotEquals | GreaterThan | LessThan
       | GreaterThanOrEqual | LessThanOrEqual
       | And | Or => true
  case _ => false
}

def isUnaryOperator(token: Token) = token match {
  case Minus | Not => true
  case _ => false
}

def isOperator(token: Token) =
  isBinOperator(token) || isUnaryOperator(token)

def getLitValue(token: Token): Option[Int] = token match {
  case Literal(value) => Some(value)
  case _ => None
}

def getIdentValue(token: Token): Option[String] = token match {
  case Identifier(value) => Some(value)
  case _ => None
}

def printTokenStream(tokens: List[Token]) = {
  for token <- tokens do
    token match {
      case EOF => print("EOF")
      case NewLine => print("\n")
      case Colon => print(":")
      case SemiColon => print(";")
      case Assign => print("=")
      case BracketOpen => print("(")
      case BracketClose => print(")")
      case Comma => print(",")
      case Arrow => print("->")
      case QuestionMark => print("?")
      case Literal(v) => print(v)
      case Identifier(v) => print(v)
      case Unknown => print("Unknown")
      case Print => print("print")
      case Let => print("let")
      case Int => print("Int")
      case Lambda => print("lambda")
      case Plus => print("+")
      case Minus => print("-")
      case Multiply => print("*")
      case Divide => print("/")
      case Equals => print("==")
      case NotEquals => print("!=")
      case GreaterThan => print(">")
      case LessThan => print("<")
      case GreaterThanOrEqual => print(">=")
      case LessThanOrEqual => print("<=")
      case And => print("&&")
      case Or => print("||")
      case Not => print("!")
    }
    print(" ")
}
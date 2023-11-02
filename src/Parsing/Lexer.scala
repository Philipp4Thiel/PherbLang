package Parsing

import scala.collection.mutable.ListBuffer

class Lexer(var source: Iterable[Char]) {
  val res = ListBuffer[Token]()

  def getTokenStream(): List[Token] = {
    val res = ListBuffer[Token]()
    val buffer = ListBuffer[Char]()

    def appendToken(token: Option[Token]): Unit = {
      getTokenFromBuffer(buffer) match
        case Some(t) => res.append(t)
        case _ =>
      buffer.clear()
      token match
        case Some(t) => res.append(t)
        case _ =>
    }

    while source.nonEmpty do
      source = source match {
        case ' ' :: tail => {
          appendToken(None)
          tail
        }
        case '\n' :: tail => {
          appendToken(Some(Token.NewLine))
          tail
        }
        case ':' :: tail => {
          appendToken(Some(Token.Colon))
          tail
        }
        case ';' :: tail => {
          appendToken(Some(Token.SemiColon))
          tail
        }
        case '=' :: '=' :: tail => {
          appendToken(Some(Token.Equals))
          tail
        }
        case '=' :: tail => {
          appendToken(Some(Token.Assign))
          tail
        }
        case '(' :: tail => {
          appendToken(Some(Token.BracketOpen))
          tail
        }
        case ')' :: tail => {
          appendToken(Some(Token.BracketClose))
          tail
        }
        case ',' :: tail => {
          appendToken(Some(Token.Comma))
          tail
        }
        case '-' :: '>' :: tail => {
          appendToken(Some(Token.Arrow))
          tail
        }
        case '?' :: tail => {
          appendToken(Some(Token.QuestionMark))
          tail
        }
        case '+' :: tail => {
          appendToken(Some(Token.Plus))
          tail
        }
        case '-' :: tail => {
          appendToken(Some(Token.Minus))
          tail
        }
        case '*' :: tail => {
          appendToken(Some(Token.Multiply))
          tail
        }
        case '/' :: tail => {
          appendToken(Some(Token.Divide))
          tail
        }
        case '!' :: '=' :: tail => {
          appendToken(Some(Token.NotEquals))
          tail
        }
        case '>' :: '=' :: tail => {
          appendToken(Some(Token.GreaterThanOrEqual))
          tail
        }
        case '>' :: tail => {
          appendToken(Some(Token.GreaterThan))
          tail
        }
        case '<' :: '=' :: tail => {
          appendToken(Some(Token.LessThanOrEqual))
          tail
        }
        case '<' :: tail => {
          appendToken(Some(Token.LessThan))
          tail
        }
        case '&' :: '&' :: tail => {
          appendToken(Some(Token.And))
          tail
        }
        case '|' :: '|' :: tail => {
          appendToken(Some(Token.Or))
          tail
        }
        case '!' :: tail => {
          appendToken(Some(Token.Not))
          tail
        }
        case c :: tail => {
          buffer.append(c)
          tail
        }
      }
    appendToken(Some(Token.EOF))
    res.toList
  }
}

def getTokenFromBuffer(buffer: Iterable[Char]): Option[Token] = {
  if buffer.isEmpty then
    return None
  val first = buffer.head
  if '0' <= first && first <= '9' then
    if buffer.tail.forall(c => '0' <= c && c <= '9') then
      val value = buffer.mkString.toInt
      Some(Token.Literal(value))
    else
      throw new LexerException(s"Illegal Literal $buffer")
  else if ('a' <= first && first <= 'z') || ('A' <= first && first <= 'Z') || '_' == first then
    if buffer.tail.forall(
      c => ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'
    ) then
      buffer.mkString match {
        case "let" => Some(Token.Let)
        case "print" => Some(Token.Print)
        case "Int" => Some(Token.Int)
        case "Lambda" => Some(Token.Lambda)
        case s => Some(Token.Identifier(s))
      }
    else
      throw new LexerException(s"Illegal identifier $buffer")
  else
    throw new LexerException(s"Illegal character $first for number/identifier")
}

def getLexerFromString(program: String) = Lexer(program.toList)
def getLexerFromFile(filePath: String) = {
  throw new NotImplementedError()
}

class LexerException(msg: String) extends RuntimeException(msg)
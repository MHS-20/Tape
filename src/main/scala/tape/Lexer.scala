package tape

case class Loc(file: String, row: Int, col: Int):
  override def toString: String = s"$file:$row:$col"

case class Token(name: String, loc: Loc):
  override def toString: String = name
  override def equals(obj: Any): Boolean = obj match
    case that: Token => this.name == that.name
    case _ => false
  override def hashCode(): Int = name.hashCode

class Lexer(source: String, filePath: String):
  private var pos = 0
  private var bol = 0
  private var row = 0
  private var peeked: Option[Token] = None

  def loc: Loc = Loc(filePath, row + 1, pos - bol + 1)

  private def advance(n: Int): Unit =
    var i = 0
    while i < n && pos < source.length do
      if source.charAt(pos) == '\n' then
        bol = pos + 1
        row += 1
      pos += 1
      i += 1

  private def skipWhile(pred: Char => Boolean): Unit =
    while pos < source.length && pred(source.charAt(pos)) do
      advance(1)

  private def takeWhile(pred: Char => Boolean): String =
    val sb = new StringBuilder
    while pos < source.length && pred(source.charAt(pos)) do
      sb.append(source.charAt(pos))
      advance(1)
    sb.result()

  private def startsWith(prefix: String): Boolean =
    pos + prefix.length <= source.length &&
      source.substring(pos, pos + prefix.length) == prefix

  private def skipPrefixIf(prefix: String): Boolean =
    if startsWith(prefix) then
      advance(prefix.length)
      true
    else false

  private def chopTokenImpl(): Option[Token] =
    var continue = true
    while continue do
      skipWhile(_.isWhitespace)
      if skipPrefixIf("//") then
        skipWhile(_ != '\n')
      else
        continue = false

    if pos >= source.length then return None

    val currentLoc = loc

    val special = "(){}[]"
    var foundSpecial = false
    var i = 0
    while i < special.length && !foundSpecial do
      val c = special.charAt(i)
      if pos < source.length && source.charAt(pos) == c then
        advance(1)
        return Some(Token(c.toString, currentLoc))
      i += 1

    if pos < source.length && source.charAt(pos) == '\'' then
      advance(1)
      val sb = new StringBuilder
      var escaped = false
      var found = false
      while pos < source.length && !found do
        val ch = source.charAt(pos)
        if !escaped && ch == '\'' then
          advance(1)
          found = true
        else if !escaped && ch == '\\' then
          escaped = true
          advance(1)
        else
          sb.append(ch)
          escaped = false
          advance(1)
      return Some(Token("'" + sb.result() + "'", currentLoc))

    val name = takeWhile(ch => !ch.isWhitespace && !"(){}[]'".contains(ch))
    if name.isEmpty then None
    else Some(Token(name, currentLoc))

  def nextToken(): Option[Token] =
    val result = peeked
    peeked = None
    result.orElse(chopTokenImpl())

  def peekToken(): Option[Token] =
    if peeked.isEmpty then peeked = chopTokenImpl()
    peeked

  def parseToken(): Either[String, Token] =
    nextToken() match
      case Some(tok) => Right(tok)
      case None => Left(s"${loc}: ERROR: expected token but reached end of input")

  def expectTokens(expected: String*): Either[String, Token] =
    parseToken().flatMap { tok =>
      if expected.contains(tok.name) then Right(tok)
      else
        val list = expected.toList match
          case Nil => ""
          case h :: Nil => h
          case h :: t =>
            val rest = t.dropRight(1).mkString(", ")
            if rest.isEmpty then s"$h, or ${t.last}"
            else s"$h, $rest, or ${t.last}"
        Left(s"${tok.loc}: ERROR: expected $list but got ${tok.name}")
    }

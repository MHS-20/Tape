package tape

import scala.collection.mutable

enum Atom:
  case Symbol(sym: Token)
  case Integer(loc: Loc, value: Long)
  case Real(loc: Loc, value: Float)
  case StringLit(loc: Loc, value: String)

  override def equals(obj: Any): Boolean = obj match
    case that: Atom => (this, that) match
      case (Symbol(a), Symbol(b)) => a == b
      case (Integer(_, a), Integer(_, b)) => a == b
      case (Real(_, a), Real(_, b)) => a == b
      case (StringLit(_, a), StringLit(_, b)) => a == b
      case _ => false
    case _ => false

  override def hashCode(): Int = this match
    case Symbol(sym) => sym.hashCode
    case Integer(_, v) => v.hashCode
    case Real(_, v) => java.lang.Float.floatToIntBits(v)
    case StringLit(_, v) => v.hashCode

  def location: Loc = this match
    case Symbol(tok)     => tok.loc
    case Integer(loc, _) => loc
    case Real(loc, _)    => loc
    case StringLit(loc, _) => loc

  def human: String = this match
    case Symbol(_)      => "symbol"
    case Integer(_, _)  => "integer value"
    case Real(_, _)     => "real value"
    case StringLit(_, _) => "string value"

  override def toString: String = this match
    case Symbol(tok)      => tok.name
    case Integer(_, v)    => v.toString
    case Real(_, v)       => v.toString
    case StringLit(_, v)  => s"'$v'"

object Atom:
  def fromToken(tok: Token): Either[String, Atom] =
    if tok.name.startsWith("'") then
      val inner = tok.name.substring(1, tok.name.length - 1)
      Right(StringLit(tok.loc, inner))
    else
      tok.name.toLongOption match
        case Some(v) => Right(Integer(tok.loc, v))
        case None =>
          tok.name.toFloatOption match
            case Some(v) => Right(Real(tok.loc, v))
            case None => Right(Symbol(tok))

extension (a: Atom)
  def expectSymbol: Result[Token] = a match
    case Atom.Symbol(tok) => Right(tok)
    case _ => Left(s"${a.location}: ERROR: expected symbol but got ${a.human} `$a`")
  def expectInteger: Result[Long] = a match
    case Atom.Integer(_, v) => Right(v)
    case _ => Left(s"${a.location}: ERROR: expected integer but got ${a.human} `$a`")
  def expectReal: Result[Float] = a match
    case Atom.Real(_, v) => Right(v)
    case _ => Left(s"${a.location}: ERROR: expected real but got ${a.human} `$a`")
  def expectString: Result[String] = a match
    case Atom.StringLit(_, v) => Right(v)
    case _ => Left(s"${a.location}: ERROR: expected string but got ${a.human} `$a`")

def expectBool(tok: Token): Either[String, Boolean] =
  tok.name match
    case "true"  => Right(true)
    case "false" => Right(false)
    case _       => Left(s"${tok.loc}: ERROR: expected boolean but got ${tok.name}")

def boolToStr(b: Boolean): String = if b then "true" else "false"

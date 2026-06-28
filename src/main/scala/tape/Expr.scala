package tape

import scala.collection.mutable.{HashMap, HashSet}
import scala.util.boundary, boundary.break

type Scope = HashMap[Token, SetExpr]
type Result[T] = Either[String, T]

enum Expr:
  case AtomExpr(atom: Atom)
  case Eval(loc: Loc, lhs: Expr, op: Expr, rhs: Expr)
  case Tuple(loc: Loc, elements: List[Expr])

  def location: Loc = this match
    case AtomExpr(atom)  => atom.location
    case Eval(loc, _, _, _) => loc
    case Tuple(loc, _)   => loc

  override def equals(obj: Any): Boolean = obj match
    case that: Expr => (this, that) match
      case (AtomExpr(a), AtomExpr(b)) => a == b
      case (Eval(_, la, _, ra), Eval(_, lb, _, rb)) => la == lb && ra == rb
      case (Tuple(_, as), Tuple(_, bs)) => as == bs
      case _ => false
    case _ => false

  override def hashCode(): Int = this match
    case AtomExpr(a) => a.hashCode
    case Eval(_, l, _, r) => l.hashCode * 31 + r.hashCode
    case Tuple(_, es) => es.hashCode

  def expectAtom: Result[Atom] = this match
    case AtomExpr(atom) => Right(atom)
    case Tuple(loc, _)  => Left(s"$loc: ERROR: expected atom but got tuple")
    case Eval(loc, _, _, _) => Left(s"$loc: ERROR: expected atom but got eval")

  def usesVar(sym: Token): Option[Token] = this match
    case AtomExpr(Atom.Symbol(s)) if s == sym => Some(s)
    case AtomExpr(_) => None
    case Eval(_, lhs, _, rhs) => lhs.usesVar(sym).orElse(rhs.usesVar(sym))
    case Tuple(_, elements) => elements.collectFirst {
      case e if e.usesVar(sym).isDefined => sym
    }

  def forceEvals: Result[Expr] = boundary:
    this match
      case AtomExpr(_) => Right(this)
      case Tuple(loc, elements) =>
        val newElements = List.newBuilder[Expr]
        for e <- elements do
          e.forceEvals match
            case Right(fe) => newElements += fe
            case l @ Left(_) => break(l)
        Right(Tuple(loc, newElements.result()))
      case Eval(loc, lhs, op, rhs) =>
        lhs.forceEvals match
          case Left(err) => break(Left(err))
          case Right(flhs) =>
            flhs.expectAtom match
              case Left(err) => break(Left(err))
              case Right(flatom) =>
                op.forceEvals match
                  case Left(err) => break(Left(err))
                  case Right(fop) =>
                    fop.expectAtom match
                      case Left(err) => break(Left(err))
                      case Right(Atom.Symbol(foptok)) =>
                        rhs.forceEvals match
                          case Left(err) => break(Left(err))
                          case Right(frhs) => evalAtomOp(flatom, foptok, frhs, loc)
                      case Right(a) => break(Left(s"${a.location}: ERROR: expected symbol for operator"))

  def substituteBindings(bindings: HashMap[Token, Expr]): Expr = this match
    case AtomExpr(Atom.Symbol(s)) =>
      bindings.getOrElse(s, this)
    case AtomExpr(_) => this
    case Eval(loc, lhs, op, rhs) =>
      Eval(loc,
        lhs.substituteBindings(bindings),
        op.substituteBindings(bindings),
        rhs.substituteBindings(bindings))
    case Tuple(loc, elements) =>
      Tuple(loc, elements.map(_.substituteBindings(bindings)))

  def patternMatch(value: Expr, scope: Scope, bindings: HashMap[Token, Expr]): Boolean =
    this match
      case AtomExpr(patternAtom) => patternAtom match
        case Atom.Symbol(patternSym) =>
          if scope.contains(patternSym) then
            bindings.get(patternSym) match
              case Some(existing) => existing == value
              case None =>
                bindings(patternSym) = value
                true
          else value match
            case AtomExpr(Atom.Symbol(valueSym)) => patternSym.name == valueSym.name
            case _ => false
        case Atom.Integer(_, pv) => value match
          case AtomExpr(Atom.Integer(_, vv)) => pv == vv
          case _ => false
        case Atom.Real(_, pv) => value match
          case AtomExpr(Atom.Real(_, vv)) => pv == vv
          case _ => false
        case Atom.StringLit(_, pv) => value match
          case AtomExpr(Atom.StringLit(_, vv)) => pv == vv
          case _ => false
      case Eval(_, _, _, _) =>
        throw new IllegalStateException("Eval in pattern matching should be forbidden upfront")
      case Tuple(_, patElements) => value match
        case Tuple(_, valElements) =>
          if patElements.length != valElements.length then return false
          patElements.zip(valElements).forall { (p, v) =>
            p.patternMatch(v, scope, bindings)
          }
        case _ => false

  def enumerate(cache: HashMap[Expr, Int]): Int =
    cache.getOrElse(this, {
      val id = cache.size
      cache(this) = id
      id
    })

  override def toString: String = this match
    case AtomExpr(atom) => atom.toString
    case Eval(_, lhs, _, rhs) => s"[$lhs + $rhs]"
    case Tuple(_, elements) => elements.mkString("(", " ", ")")

object Expr:
  def parse(lexer: Lexer): Result[Expr] =
    lexer.parseToken().flatMap { tok =>
      tok.name match
        case "(" =>
          var elements = List.empty[Expr]
          var error: Option[String] = None
          while
            lexer.peekToken() match
              case Some(t) if t.name == ")" => false
              case _ =>
                parse(lexer) match
                  case Right(e) => elements = elements :+ e; true
                  case Left(err) => error = Some(err); false
          do ()
          error match
            case Some(err) => Left(err)
            case None => lexer.expectTokens(")").map(_ => Tuple(tok.loc, elements))
        case "[" =>
          parse(lexer) match
            case Left(err) => Left(err)
            case Right(lhs) =>
              parse(lexer) match
                case Left(err) => Left(err)
                case Right(op) =>
                  parse(lexer) match
                    case Left(err) => Left(err)
                    case Right(rhs) =>
                      lexer.expectTokens("]").map(_ => Eval(tok.loc, lhs, op, rhs))
        case _ =>
          Atom.fromToken(tok).map(AtomExpr.apply)
    }

def forceEvals(e: Expr): Result[Expr] = e.forceEvals

def evalAtomOp(lhs: Atom, op: Token, rhsExpr: Expr, loc: Loc): Result[Expr] =
  lhs match
    case Atom.Integer(_, lv) =>
      for
        ratom <- rhsExpr.expectAtom
        rv <- ratom match
          case Atom.Integer(_, v) => Right(v)
          case _ => Left(s"${ratom.location}: ERROR: expected integer got ${ratom.human}")
      yield evalIntegerOp(lv, rv, op, loc)

    case Atom.Symbol(sym) =>
      for
        lb <- expectBool(sym)
        ratom <- rhsExpr.expectAtom
        rsym <- ratom match
          case Atom.Symbol(s) => Right(s)
          case _ => Left(s"${ratom.location}: ERROR: expected symbol")
        rb <- expectBool(rsym)
      yield evalBoolOp(lb, rb, op, loc)

    case Atom.Real(_, lv) =>
      for
        ratom <- rhsExpr.expectAtom
        rv <- ratom match
          case Atom.Real(_, v) => Right(v)
          case _ => Left(s"${ratom.location}: ERROR: expected real got ${ratom.human}")
      yield evalRealOp(lv, rv, op, loc)

    case Atom.StringLit(_, lv) =>
      for
        ratom <- rhsExpr.expectAtom
        rv <- ratom match
          case Atom.StringLit(_, v) => Right(v)
          case _ => Left(s"${ratom.location}: ERROR: expected string got ${ratom.human}")
      yield evalStringOp(lv, rv, op, loc)

def evalIntegerOp(lhs: Long, rhs: Long, op: Token, loc: Loc): Expr =
  import scala.util.Try
  op.name match
    case "+"  => Try(Expr.AtomExpr(Atom.Integer(loc, Math.addExact(lhs, rhs)))).getOrElse(Expr.AtomExpr(Atom.Integer(loc, lhs + rhs)))
    case "-"  => Expr.AtomExpr(Atom.Integer(loc, lhs - rhs))
    case "*"  => Expr.AtomExpr(Atom.Integer(loc, lhs * rhs))
    case "/"  => Expr.AtomExpr(Atom.Integer(loc, lhs / rhs))
    case "%"  => Expr.AtomExpr(Atom.Integer(loc, lhs % rhs))
    case ">"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs > rhs), loc)))
    case ">=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs >= rhs), loc)))
    case "<"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs < rhs), loc)))
    case "<=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs <= rhs), loc)))
    case "==" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs == rhs), loc)))
    case "!=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs != rhs), loc)))
    case _    => throw new IllegalArgumentException(s"unexpected integer operator ${op.name}")

def evalBoolOp(lhs: Boolean, rhs: Boolean, op: Token, loc: Loc): Expr =
  op.name match
    case "||" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs || rhs), loc)))
    case "&&" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs && rhs), loc)))
    case "==" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs == rhs), loc)))
    case "!=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs != rhs), loc)))
    case _    => throw new IllegalArgumentException(s"unexpected boolean operator ${op.name}")

def evalRealOp(lhs: Float, rhs: Float, op: Token, loc: Loc): Expr =
  op.name match
    case "+"  => Expr.AtomExpr(Atom.Real(loc, lhs + rhs))
    case "-"  => Expr.AtomExpr(Atom.Real(loc, lhs - rhs))
    case "*"  => Expr.AtomExpr(Atom.Real(loc, lhs * rhs))
    case "/"  => Expr.AtomExpr(Atom.Real(loc, lhs / rhs))
    case "%"  => Expr.AtomExpr(Atom.Real(loc, lhs % rhs))
    case ">"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs > rhs), loc)))
    case ">=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs >= rhs), loc)))
    case "<"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs < rhs), loc)))
    case "<=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs <= rhs), loc)))
    case "==" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs == rhs), loc)))
    case "!=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs != rhs), loc)))
    case _    => throw new IllegalArgumentException(s"unexpected real operator ${op.name}")

def evalStringOp(lhs: String, rhs: String, op: Token, loc: Loc): Expr =
  op.name match
    case "+"  => Expr.AtomExpr(Atom.StringLit(loc, lhs + rhs))
    case ">"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs > rhs), loc)))
    case ">=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs >= rhs), loc)))
    case "<"  => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs < rhs), loc)))
    case "<=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs <= rhs), loc)))
    case "==" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs == rhs), loc)))
    case "!=" => Expr.AtomExpr(Atom.Symbol(Token(boolToStr(lhs != rhs), loc)))
    case _    => throw new IllegalArgumentException(s"unexpected string operator ${op.name}")

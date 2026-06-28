package tape

import scala.collection.mutable.{HashMap, HashSet}

enum SetExpr:
  case Named(name: Token)
  case Enclosed(loc: Loc, inner: SetExpr)
  case Anonymous(loc: Loc, elements: HashSet[Expr])
  case IntegerSet(loc: Loc)
  case RealSet(loc: Loc)
  case StringSet(loc: Loc)
  case Union(lhs: SetExpr, rhs: SetExpr)
  case Diff(lhs: SetExpr, rhs: SetExpr)
  case Product(elements: List[SetExpr])

  def setLoc: Loc = this match
    case Named(tok)       => tok.loc
    case Enclosed(loc, _) => loc
    case Anonymous(loc, _) => loc
    case IntegerSet(loc)  => loc
    case RealSet(loc)     => loc
    case StringSet(loc)   => loc
    case Union(lhs, _)    => lhs.setLoc
    case Diff(lhs, _)     => lhs.setLoc
    case Product(h :: _)  => h.setLoc
    case Product(Nil)     => throw new IllegalStateException("empty product")

  override def toString: String = this match
    case Named(tok)      => tok.name
    case Enclosed(_, inner) => s"($inner)"
    case Anonymous(_, elements) => elements.mkString("{", " ", "}")
    case IntegerSet(_)   => "Integer"
    case RealSet(_)      => "Real"
    case StringSet(_)     => "String"
    case Union(l, r)     => s"$l + $r"
    case Diff(l, r)      => s"$l - $r"
    case Product(es)     => es.mkString(" * ")

object SetExpr:
  def parsePrimary(lexer: Lexer, sets: HashMap[Token, SetExpr]): Result[SetExpr] =
    lexer.peekToken() match
      case None =>
        Left(s"${lexer.loc}: ERROR: expected token but reached end of input")
      case Some(tok) => tok.name match
        case "{" =>
          lexer.nextToken()
          parseAnonymous(lexer).map { els =>
            Anonymous(tok.loc, els)
          }
        case "(" =>
          val open = lexer.nextToken().get
          for
            inner <- parse(lexer, sets)
            _     <- lexer.expectTokens(")")
          yield Enclosed(open.loc, inner)
        case _ =>
          lexer.nextToken()
          Atom.fromToken(tok).flatMap {
            case Atom.Integer(loc, _) => Left(s"$loc: ERROR: integer value is not a set expression")
            case Atom.Real(loc, _)    => Left(s"$loc: ERROR: real value is not a set expression")
            case Atom.StringLit(loc, _) => Left(s"$loc: ERROR: string value is not a set expression")
            case Atom.Symbol(sym) => sym.name match
              case "Integer" => Right(IntegerSet(sym.loc))
              case "Real"    => Right(RealSet(sym.loc))
              case "String"  => Right(StringSet(sym.loc))
              case _ =>
                if sets.contains(sym) then Right(Named(sym))
                else Left(s"${sym.loc}: ERROR: set $sym does not exist")
          }

  def parseAnonymous(lexer: Lexer): Result[HashSet[Expr]] =
    val set = new HashSet[Expr]
    while
      lexer.peekToken() match
        case Some(tok) if tok.name == "}" => false
        case _ =>
          Expr.parse(lexer).flatMap(_.forceEvals) match
            case Right(value) =>
              if set.contains(value) then
                Left(s"${value.location}: ERROR: duplicate value in set")
              else
                set.add(value)
                Right(())
            case Left(err) => Left(err)
          true
    do ()
    lexer.expectTokens("}").map(_ => set)

  def parseProduct(lexer: Lexer, sets: HashMap[Token, SetExpr]): Result[SetExpr] =
    var elements = List.empty[SetExpr]
    var error: Option[String] = None
    parsePrimary(lexer, sets) match
      case Right(first) =>
        elements = first :: elements
        var continue = true
        while continue && error.isEmpty do
          lexer.peekToken() match
            case Some(tok) if tok.name == "*" =>
              lexer.nextToken()
              parsePrimary(lexer, sets) match
                case Right(e) => elements = e :: elements
                case Left(err) => error = Some(err)
            case _ => continue = false
        error match
          case Some(err) => Left(err)
          case None =>
            elements = elements.reverse
            if elements.length == 1 then Right(elements.head)
            else Right(Product(elements))
      case Left(err) => Left(err)

  def parse(lexer: Lexer, sets: HashMap[Token, SetExpr]): Result[SetExpr] =
    parseProduct(lexer, sets).flatMap { init =>
      var lhs = init
      var continue = true
      var error: Option[String] = None
      while continue && error.isEmpty do
        lexer.peekToken() match
          case Some(tok) if tok.name == "+" =>
            lexer.nextToken()
            parseProduct(lexer, sets) match
              case Right(rhs) => lhs = Union(lhs, rhs)
              case Left(err) => error = Some(err)
          case Some(tok) if tok.name == "-" =>
            lexer.nextToken()
            parseProduct(lexer, sets) match
              case Right(rhs) => lhs = Diff(lhs, rhs)
              case Left(err) => error = Some(err)
          case _ => continue = false
      error match
        case Some(err) => Left(err)
        case None => Right(lhs)
    }

  def contains(self: SetExpr, sets: HashMap[Token, SetExpr], element: Expr): Boolean =
    self match
      case Enclosed(_, inner) => contains(inner, sets, element)
      case Product(prodElements) => element match
        case Expr.Tuple(_, elements) =>
          if elements.length != prodElements.length then false
          else elements.zip(prodElements).forall { (e, s) => contains(s, sets, e) }
        case _ => false
      case Union(lhs, rhs) => contains(lhs, sets, element) || contains(rhs, sets, element)
      case Diff(lhs, rhs)  => contains(lhs, sets, element) && !contains(rhs, sets, element)
      case Anonymous(_, elements) => elements.contains(element)
      case IntegerSet(_) => element match
        case Expr.AtomExpr(Atom.Integer(_, _)) => true
        case _ => false
      case RealSet(_) => element match
        case Expr.AtomExpr(Atom.Real(_, _)) => true
        case _ => false
      case StringSet(_) => element match
        case Expr.AtomExpr(Atom.StringLit(_, _)) => true
        case _ => false
      case Named(name) =>
        sets.get(name) match
          case Some(se) => contains(se, sets, element)
          case None => throw new IllegalArgumentException(s"undefined set $name")

  def expand(self: SetExpr, sets: HashMap[Token, SetExpr]): Result[HashSet[Expr]] =
    self match
      case Enclosed(_, inner) => expand(inner, sets)
      case Product(elements) =>
        var expanded = List.empty[HashSet[Expr]]
        var error: Option[String] = None
        val iter = elements.iterator
        while iter.hasNext && error.isEmpty do
          expand(iter.next(), sets) match
            case Right(s) => expanded = expanded :+ s
            case Left(err) => error = Some(err)
        error match
          case Some(err) => Left(err)
          case None =>
            val result = new HashSet[Expr]
            expandProduct(expanded, self.setLoc, List.empty, result)
            Right(result)
      case Union(lhs, rhs) =>
        for
          l <- expand(lhs, sets)
          r <- expand(rhs, sets)
        yield l ++ r
      case Diff(lhs, rhs) =>
        for
          l <- expand(lhs, sets)
          r <- expand(rhs, sets)
        yield l -- r
      case Anonymous(_, elements) => Right(elements)
      case IntegerSet(loc) => Left(s"$loc: cannot expand infinite set Integer")
      case RealSet(loc)    => Left(s"$loc: cannot expand infinite set Real")
      case StringSet(loc)  => Left(s"$loc: cannot expand infinite set String")
      case Named(name) =>
        sets.get(name) match
          case Some(se) => expand(se, sets)
          case None => Left(s"${name.loc}: undefined set $name")

  def expandProduct(
    product: List[HashSet[Expr]],
    loc: Loc,
    current: List[Expr],
    result: HashSet[Expr]
  ): Unit = product match
    case head :: tail =>
      for element <- head do
        expandProduct(tail, loc, current :+ element, result)
    case Nil =>
      result.add(Expr.Tuple(loc, current))

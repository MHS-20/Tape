package tape

import scala.collection.mutable.{HashMap, HashSet}

case class Case(
  keyword: Token,
  state: Expr,
  read: Expr,
  write: Expr,
  step: Expr,
  next: Expr
):
  def substituteBindings(bindings: HashMap[Token, Expr]): Case =
    Case(
      keyword,
      state.substituteBindings(bindings),
      read.substituteBindings(bindings),
      write.substituteBindings(bindings),
      step.substituteBindings(bindings),
      next.substituteBindings(bindings)
    )

enum Statement:
  case StmtCase(case0: Case)
  case StmtBlock(statements: List[Statement])
  case StmtFor(vars: List[Token], set: SetExpr, body: Statement)

  def substituteBindings(bindings: HashMap[Token, Expr]): Statement = this match
    case StmtCase(c) => StmtCase(c.substituteBindings(bindings))
    case StmtBlock(ss) => StmtBlock(ss.map(_.substituteBindings(bindings)))
    case StmtFor(vars, set, body) => StmtFor(vars, set, body.substituteBindings(bindings))

  override def toString: String = this match
    case StmtCase(c) => s"${c.keyword} ${c.state} ${c.read} ${c.write} ${c.step} ${c.next}"
    case StmtBlock(ss) => ss.mkString("{", " ", "}")
    case StmtFor(vars, set, body) =>
      s"for ${vars.mkString(" ")} in $set $body"

  def matchCaseScoped(
    scope: Scope,
    sets: HashMap[Token, SetExpr],
    state: Expr,
    read: Expr
  ): Result[Option[(Expr, Expr, Expr)]] = this match
    case StmtCase(case0) =>
      val bindings = new HashMap[Token, Expr]
      if !case0.state.forceEvals.toOption.exists(_.patternMatch(state, scope, bindings)) then
        return Right(None)
      if !case0.read.forceEvals.toOption.exists(_.patternMatch(read, scope, bindings)) then
        return Right(None)

      var scopeError = false
      val scopeIter = scope.iterator
      while scopeIter.hasNext && !scopeError do
        val (sym, set) = scopeIter.next()
        bindings.get(sym) match
          case Some(value) =>
            if !SetExpr.contains(set, sets, value) then scopeError = true
          case None =>
            throw new IllegalStateException("unused variable at runtime - sanity check not performed")
      if scopeError then return Right(None)

      val w = case0.write.substituteBindings(bindings).forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)
      val s = case0.step.substituteBindings(bindings).forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)
      val n = case0.next.substituteBindings(bindings).forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)

      Right(Some((w, s, n)))

    case StmtBlock(statements) =>
      var result: Option[Result[Option[(Expr, Expr, Expr)]]] = None
      val iter = statements.iterator
      while iter.hasNext && result.isEmpty do
        matchCaseScoped(scope, sets, state, read) match
          case r @ Right(Some(_)) => result = Some(r)
          case Right(None) => ()
          case l @ Left(_) => result = Some(l)
      result.getOrElse(Right(None))

    case StmtFor(vars, set, body) =>
      for v <- vars do
        if scope.contains(v) then
          throw new IllegalStateException("shadowed variable at runtime - sanity check not performed")
        scope(v) = set
      val result = body.matchCaseScoped(scope, sets, state, read)
      for v <- vars do scope.remove(v)
      result

  def matchCase(
    sets: HashMap[Token, SetExpr],
    state: Expr,
    read: Expr
  ): Result[Option[(Expr, Expr, Expr)]] =
    matchCaseScoped(new HashMap, sets, state, read)

  def expand(
    bindings: HashMap[Token, Expr],
    cache: HashMap[Expr, Int],
    sets: HashMap[Token, SetExpr],
    enumerate: Boolean
  ): Result[Unit] = this match
    case StmtCase(case0) =>
      val c = case0.substituteBindings(bindings)
      val wr = c.write.forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)
      val sp = c.step.forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)
      val nx = c.next.forceEvals match
        case Right(e) => e
        case Left(err) => return Left(err)
      val st = if enumerate then enumExpr(c.state, cache).toString else c.state.toString
      val rd = if enumerate then enumExpr(c.read, cache).toString else c.read.toString
      val wrStr = if enumerate then enumExpr(wr, cache).toString else wr.toString
      val spStr = if enumerate then enumExpr(sp, cache).toString else sp.toString
      val nxStr = if enumerate then enumExpr(nx, cache).toString else nx.toString
      println(s"${c.keyword} $st $rd $wrStr $spStr $nxStr")
      Right(())

    case StmtBlock(statements) =>
      var error: Option[String] = None
      val iter = statements.iterator
      while iter.hasNext && error.isEmpty do
        iter.next().expand(bindings, cache, sets, enumerate) match
          case Left(err) => error = Some(err)
          case Right(_) => ()
      error match
        case Some(err) => Left(err)
        case None => Right(())

    case StmtFor(vars, set, body) =>
      SetExpr.expand(set, sets) match
        case Right(elements) =>
          var error: Option[String] = None
          val iter = elements.iterator
          while iter.hasNext && error.isEmpty do
            val element = iter.next()
            for v <- vars do
              if bindings.contains(v) then
                throw new IllegalStateException("shadowed variable at expansion - sanity check not performed")
              bindings(v) = element
            body.expand(bindings, cache, sets, enumerate) match
              case Left(err) => error = Some(err)
              case Right(_) => ()
            for v <- vars do bindings.remove(v)
          error match
            case Some(err) => Left(err)
            case None => Right(())
        case Left(err) => Left(err)

  def sanityCheck(scope: Scope): Result[Unit] = this match
    case StmtCase(case0) =>
      val unused = scope.filter { (v, _) =>
        case0.state.usesVar(v).isEmpty && case0.read.usesVar(v).isEmpty
      }
      if unused.nonEmpty then
        val sorted = unused.keys.toList.sortBy(_.loc.row)
        println(s"${case0.keyword.loc}: ERROR: not all variables in the scope are used in the input of the case")
        for v <- sorted do
          println(s"${v.loc}: NOTE: unused variable $v")
        Left("unused variables")
      else Right(())

    case StmtBlock(statements) =>
      var error: Option[String] = None
      val iter = statements.iterator
      while iter.hasNext && error.isEmpty do
        iter.next().sanityCheck(scope) match
          case Left(err) => error = Some(err)
          case Right(_) => ()
      error match
        case Some(err) => Left(err)
        case None => Right(())

    case StmtFor(vars, set, body) =>
      var shadowError = false
      for v <- vars do
        if scope.contains(v) then
          println(s"${v.loc}: ERROR: $v shadows another name in the higher scope")
          shadowError = true
        scope(v) = set
      if shadowError then return Left("shadowed variable")
      val result = body.sanityCheck(scope)
      for v <- vars do scope.remove(v)
      result

  def topLevelSanityCheck: Result[Unit] = sanityCheck(new HashMap)

def enumExpr(e: Expr, cache: HashMap[Expr, Int]): Int =
  cache.getOrElseUpdate(e, cache.size)

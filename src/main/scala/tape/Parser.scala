package tape

import scala.collection.mutable.{HashMap, HashSet}

case class Program(
  sets: HashMap[Token, SetExpr],
  statements: List[Statement],
  runs: List[Run]
)

case class Run(
  kind: RunKind,
  keyword: Token,
  state: Expr,
  left: List[Expr],
  right: List[Expr]
):
  def expand(cache: HashMap[Expr, Int], enumerate: Boolean): Unit =
    val st = if enumerate then enumExpr(state, cache).toString else state.toString
    print(s"$kind $st {")
    val all = left.reverse ++ right
    for (e, i) <- all.zipWithIndex do
      if i > 0 then print(" ")
      if enumerate then print(enumExpr(e, cache))
      else print(e)
    println("}")

object Program:
  def parseFile(path: String): Result[Program] =
    val source =
      try Right(scala.io.Source.fromFile(path).mkString)
      catch case e: Exception => Left(s"ERROR: could not read file $path: ${e.getMessage}")
    source.flatMap(s => parse(s, path))

  def parse(source: String, filePath: String): Result[Program] =
    val lexer = new Lexer(source, filePath)
    val sets = new HashMap[Token, SetExpr]
    var statements = List.empty[Statement]
    var runs = List.empty[Run]

    while lexer.peekToken().isDefined do
      val key = lexer.peekToken().get
      key.name match
        case "run" | "trace" =>
          parseRun(lexer) match
            case Right(r) => runs = runs :+ r
            case Left(err) => return Left(err)

        case "case" | "for" | "{" =>
          parseStatement(lexer, sets) match
            case Right(s) => statements = statements :+ s
            case Left(err) => return Left(err)

        case "let" =>
          lexer.nextToken()
          val symTok = lexer.parseToken() match
            case Right(t) => t
            case Left(err) => return Left(err)
          val nameAtom = Atom.fromToken(symTok) match
            case Right(a) => a
            case Left(err) => return Left(err)
          val name = nameAtom match
            case Atom.Symbol(s) => s
            case _ => return Left(s"${nameAtom.location}: ERROR: set name may not be ${nameAtom.human}")

          name.name match
            case "Integer" | "Real" | "String" =>
              return Left(s"${name.loc}: ERROR: redefinition of magical set $name")
            case _ => ()

          sets.get(name) match
            case Some(existing) =>
              return Left(s"${name.loc}: ERROR: redefinition of set $name\n${existing.setLoc}: NOTE: first defined here")
            case None => ()

          SetExpr.parse(lexer, sets) match
            case Right(se) => sets(name) = se
            case Left(err) => return Left(err)

        case _ =>
          return Left(s"${key.loc}: ERROR: unknown keyword ${key.name}")

    Right(Program(sets, statements, runs))

  def parseRun(lexer: Lexer): Result[Run] =
    val keyword = lexer.expectTokens("run", "trace") match
      case Right(t) => t
      case Left(err) => return Left(err)
    val kind = RunKind.fromName(keyword.name).get

    val state = Expr.parse(lexer).flatMap(_.forceEvals) match
      case Right(e) => e
      case Left(err) => return Left(err)

    val openCurly = lexer.expectTokens("{") match
      case Right(t) => t
      case Left(err) => return Left(err)

    var tapeSeq = List.empty[Expr]
    var continue = true
    while continue do
      lexer.peekToken() match
        case Some(tok) if tok.name == "}" =>
          continue = false
        case _ =>
          Expr.parse(lexer).flatMap(_.forceEvals) match
            case Right(e) => tapeSeq = tapeSeq :+ e
            case Left(err) => return Left(err)

    lexer.expectTokens("}") match
      case Left(err) => return Left(err)
      case Right(_) => ()

    val result = lexer.peekToken() match
      case Some(tok) if tok.name == "{" =>
        val openCurly2 = lexer.nextToken().get
        var rightSeq = List.empty[Expr]
        var cont = true
        while cont do
          lexer.peekToken() match
            case Some(t) if t.name == "}" => cont = false
            case _ =>
              Expr.parse(lexer).flatMap(_.forceEvals) match
                case Right(e) => rightSeq = rightSeq :+ e
                case Left(err) => return Left(err)
        lexer.expectTokens("}") match
          case Left(err) => return Left(err)
          case Right(_) => ()
        if tapeSeq.isEmpty && rightSeq.isEmpty then
          Left(s"${openCurly.loc}: ERROR: tape may not be empty")
        else
          Right(Run(kind, keyword, state, tapeSeq.reverse, rightSeq))
      case _ =>
        if tapeSeq.isEmpty then
          Left(s"${openCurly.loc}: ERROR: tape may not be empty")
        else
          Right(Run(kind, keyword, state, List.empty, tapeSeq))

    result

  def parseStatement(lexer: Lexer, sets: HashMap[Token, SetExpr]): Result[Statement] =
    val key = lexer.expectTokens("case", "for", "{") match
      case Right(t) => t
      case Left(err) => return Left(err)

    key.name match
      case "case" =>
        val state = Expr.parse(lexer) match
          case Right(e) => e
          case Left(err) => return Left(err)
        val read = Expr.parse(lexer) match
          case Right(e) => e
          case Left(err) => return Left(err)
        val write = Expr.parse(lexer) match
          case Right(e) => e
          case Left(err) => return Left(err)
        val step = Expr.parse(lexer) match
          case Right(e) => e
          case Left(err) => return Left(err)
        val next = Expr.parse(lexer) match
          case Right(e) => e
          case Left(err) => return Left(err)
        Right(Statement.StmtCase(Case(key, state, read, write, step, next)))

      case "{" =>
        var statements = List.empty[Statement]
        var continue = true
        while continue do
          lexer.peekToken() match
            case Some(tok) if tok.name == "}" => continue = false
            case _ =>
              parseStatement(lexer, sets) match
                case Right(s) => statements = statements :+ s
                case Left(err) => return Left(err)
        lexer.expectTokens("}") match
          case Right(_) => Right(Statement.StmtBlock(statements))
          case Left(err) => Left(err)

      case "for" =>
        var vars = List.empty[Token]
        var continue = true
        while continue do
          lexer.peekToken() match
            case Some(tok) if tok.name == "in" => continue = false
            case _ =>
              Expr.parse(lexer) match
                case Right(Expr.AtomExpr(Atom.Symbol(sym))) =>
                  vars = vars :+ sym
                case Right(Expr.AtomExpr(other)) =>
                  return Left(s"${other.location}: ERROR: ${other.human} may not be used as variable name")
                case Right(_) =>
                  return Left(s"${lexer.loc}: ERROR: pattern matching in universal quantifiers is not supported")
                case Left(err) => return Left(err)

        val inTok = lexer.expectTokens("in") match
          case Right(t) => t
          case Left(err) => return Left(err)

        val set = SetExpr.parse(lexer, sets) match
          case Right(s) => s
          case Left(err) => return Left(err)

        val inner = parseStatement(lexer, sets) match
          case Right(s) => s
          case Left(err) => return Left(err)

        Right(vars.foldRight(inner) { (v, body) =>
          Statement.StmtFor(List(v), set, body)
        })

      case _ => Left("unreachable")

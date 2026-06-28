package tape

import scala.collection.mutable.{HashMap, HashSet}
import scala.io.Source

object Main:
  def main(args: Array[String]): Unit =
    if args.length < 1 then
      usage()
      System.exit(1)

    val result = args(0) match
      case "run"    => runCommand(args.drop(1))
      case "expand" => expandCommand(args.drop(1))
      case "lex"    => lexCommand(args.drop(1))
      case "help"   => helpCommand(args.drop(1)); Right(())
      case cmd      => System.err.println(s"ERROR: unknown command $cmd"); Left(())

    result match
      case Right(_) => ()
      case Left(_)  => System.exit(1)

  def usage(): Unit =
    System.err.println("Usage: tape <command> [ARGUMENTS]")
    System.err.println("Commands:")
    System.err.println("    run <input>            - Run the tape program")
    System.err.println("    expand [--enum] <input> - Expand all quantifiers inline")
    System.err.println("    lex <input>            - Dump lexer tokens")
    System.err.println("    help [command]         - Print help")

  def helpCommand(args: Array[String]): Unit =
    if args.length > 0 then
      args(0) match
        case "run" =>
          System.err.println("Usage: tape run <input.tape>")
        case "expand" =>
          System.err.println("Usage: tape expand [--enum] <input.tape>")
        case "lex" =>
          System.err.println("Usage: tape lex <input.tape>")
        case _ => usage()
    else usage()

  def runCommand(args: Array[String]): Either[Unit, Unit] =
    if args.length < 1 then
      System.err.println("ERROR: no input file provided")
      return Left(())

    val path = args(0)
    Program.parseFile(path) match
      case Left(err) =>
        System.err.println(err)
        Left(())
      case Right(prog) =>
        var checkError = false
        val stmtsIter = prog.statements.iterator
        while stmtsIter.hasNext && !checkError do
          stmtsIter.next().topLevelSanityCheck match
            case Left(_) => checkError = true
            case Right(_) => ()

        if checkError then return Left(())

        var runError = false
        val runsIter = prog.runs.iterator
        while runsIter.hasNext && !runError do
          val run = runsIter.next()
          println(s"${run.keyword.loc}: ${run.kind}")

          val tape = new TuringTape(run.left, run.right)
          val machine = new Machine(run.state, tape)

          while !machine.halt && !runError do
            if run.kind == RunKind.Trace then machine.trace()
            machine.halt = true
            machine.next(prog.statements, prog.sets) match
              case Right(_) => ()
              case Left(err) =>
                System.err.println(err)
                runError = true

        if runError then Left(()) else Right(())

  def expandCommand(args: Array[String]): Either[Unit, Unit] =
    var enumerate = false
    var sourcePath: Option[String] = None

    var argError = false
    val argsIter = args.iterator
    while argsIter.hasNext && !argError do
      argsIter.next() match
        case "--enum" => enumerate = true
        case arg =>
          if sourcePath.isDefined then
            System.err.println("ERROR: interpreting several files is not supported")
            argError = true
          else sourcePath = Some(arg)

    if argError then return Left(())

    sourcePath match
      case None =>
        System.err.println("ERROR: no input is provided")
        Left(())
      case Some(path) =>
        Program.parseFile(path) match
          case Left(err) =>
            System.err.println(err)
            Left(())
          case Right(prog) =>
            var checkError = false
            val stmtsIter = prog.statements.iterator
            while stmtsIter.hasNext && !checkError do
              stmtsIter.next().topLevelSanityCheck match
                case Left(_) => checkError = true
                case Right(_) => ()

            if checkError then return Left(())

            val cache = new HashMap[Expr, Int]
            var expandError = false
            val expIter = prog.statements.iterator
            while expIter.hasNext && !expandError do
              expIter.next().expand(new HashMap, cache, prog.sets, enumerate) match
                case Left(err) =>
                  System.err.println(err)
                  expandError = true
                case Right(_) => ()

            if !expandError then
              for run <- prog.runs do
                run.expand(cache, enumerate)

              if enumerate then
                val table = cache.toList.sortBy(_._2)
                for (expr, id) <- table do
                  println(s"// $id = $expr")

              Right(())
            else Left(())

  def lexCommand(args: Array[String]): Either[Unit, Unit] =
    if args.length < 1 then
      System.err.println("ERROR: no input file provided")
      return Left(())

    val path = args(0)
    val source = try Source.fromFile(path).mkString
      catch case e: Exception =>
        System.err.println(s"ERROR: could not read file $path: ${e.getMessage}")
        return Left(())

    val lexer = new Lexer(source, path)
    while
      lexer.nextToken() match
        case Some(tok) =>
          println(s"${tok.loc}: ${tok.name}")
          true
        case None => false
    do ()

    Right(())

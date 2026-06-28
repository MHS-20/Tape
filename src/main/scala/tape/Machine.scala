package tape

import scala.collection.mutable.{ArrayBuffer, HashMap}

class TuringTape(
  initialLeft: List[Expr],
  initialRight: List[Expr]
):
  val leftDefault: Expr =
    initialLeft.headOption.orElse(initialRight.headOption).getOrElse(
      throw new IllegalArgumentException("tape must have at least one element")
    )
  val rightDefault: Expr =
    initialRight.lastOption.orElse(initialLeft.lastOption).get

  val left: ArrayBuffer[Expr] = ArrayBuffer.from(initialLeft)
  val right: ArrayBuffer[Expr] = ArrayBuffer.from(initialRight)

  def apply(index: Int): Expr =
    if index >= 0 then
      if index >= right.length then rightDefault
      else right(index)
    else
      val i = -index - 1
      if i >= left.length then leftDefault
      else left(i)

  def update(index: Int, value: Expr): Unit =
    if index >= 0 then right(index) = value
    else left(-index - 1) = value

  def touch(index: Int): Unit =
    if index >= 0 then
      while index >= right.length do right.append(rightDefault)
    else
      val i = -index - 1
      while i >= left.length do left.append(leftDefault)

  def allCells: List[Expr] = left.reverse.toList ++ right.toList

enum RunKind:
  case Run, Trace

  override def toString: String = this match
    case Run   => "run"
    case Trace => "trace"

object RunKind:
  def fromName(name: String): Option[RunKind] = name match
    case "run"   => Some(Run)
    case "trace" => Some(Trace)
    case _       => None

class Machine(var state: Expr, val tape: TuringTape):
  var head: Int = 0
  var halt: Boolean = false

  def next(statements: List[Statement], sets: HashMap[Token, SetExpr]): Result[Unit] =
    var error: Option[String] = None
    var found = false
    val stmts = statements.iterator
    while stmts.hasNext && !found && error.isEmpty do
      stmts.next().matchCase(sets, state, tape(head)) match
        case Right(Some((write, step, next))) =>
          write.forceEvals match
            case Right(w) =>
              tape(head) = w
              step.expectAtom.flatMap(_.expectSymbol) match
                case Right(stepSym) =>
                  stepSym.name match
                    case "<-" =>
                      head -= 1
                      tape.touch(head)
                    case "->" =>
                      head += 1
                      tape.touch(head)
                    case "." => ()
                    case "!" => printTape()
                    case _ =>
                      error = Some(s"${stepSym.loc}: ERROR: unknown step action $stepSym")
                case Left(err) => error = Some(err)
              if error.isEmpty then
                state = next
                halt = false
                found = true
            case Left(err) => error = Some(err)
        case Right(None) => ()
        case Left(err) => error = Some(err)
    error match
      case Some(err) => Left(err)
      case None => Right(())

  def printTape(): Unit =
    val cells = tape.left.reverseIterator ++ tape.right.iterator
    println(cells.mkString(" "))

  def trace(): Unit =
    val buffer = new StringBuilder
    buffer.append(s"$state: ")

    var headBegin = 0
    var headEnd = 0

    val cells = tape.left.zipWithIndex.map((x, i) => (-i - 1, x)).reverse ++
                tape.right.zipWithIndex.map((x, i) => (i, x))

    for ((i, expr), idx) <- cells.zipWithIndex do
      if idx > 0 then buffer.append(' ')
      if i == head then headBegin = buffer.length
      buffer.append(expr.toString)
      if i == head then headEnd = buffer.length

    println(buffer.toString)
    val cursorLine = (" " * headBegin) + ("^" * (headEnd - headBegin))
    println(cursorLine)

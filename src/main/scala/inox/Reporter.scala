/* Copyright 2009-2016 EPFL, Lausanne */

package inox

import utils._

abstract class DebugSection(val name: String)

abstract class Reporter(val debugSections: Set[DebugSection]) {

  abstract class Severity
  case object INFO    extends Severity
  case object WARNING extends Severity
  case object ERROR   extends Severity
  case object FATAL   extends Severity
  case object INTERNAL extends Severity
  case class  DEBUG(section: DebugSection) extends Severity

  case class Message(severity: Severity, position: Position, msg: Any)

  private var _errorCount : Int = 0
  private var _warningCount : Int = 0

  final def errorCount : Int = _errorCount
  final def warningCount : Int = _warningCount

  def account(msg: Message): Message = {
    msg.severity match {
      case WARNING                  => _warningCount += 1
      case ERROR | FATAL | INTERNAL => _errorCount += 1
      case _                        =>
    }

    msg
  }

  def emit(msg: Message): Unit

  def onFatal(msg: String = ""): Nothing = throw FatalError(msg)

  def onCompilerProgress(current: Int, total: Int) = {}

  final def info(pos: Position, msg: Any): Unit    = emit(account(Message(INFO, pos, msg)))
  final def warning(pos: Position, msg: Any): Unit = emit(account(Message(WARNING, pos, msg)))
  final def error(pos: Position, msg: Any): Unit   = emit(account(Message(ERROR, pos, msg)))
  final def title(pos: Position, msg: Any): Unit   = emit(account(Message(INFO, pos, Console.BOLD + msg + Console.RESET)))

  final def fatalError(pos: Position, msg: Any): Nothing = {
    emit(account(Message(FATAL, pos, msg)))
    onFatal(msg.toString)
  }

  final def internalError(pos: Position, msg: Any) : Nothing = {
    emit(account(Message(INTERNAL, pos, msg.toString +
      "\nPlease inform the authors of Inox about this message"
    )))
    onFatal(msg.toString)
  }

  final def internalAssertion(cond : Boolean, pos: Position, msg : Any) : Unit = {
    if (!cond) internalError(pos,msg)
  }

  def terminateIfError() = {
    if (errorCount > 0) {
      _errorCount = 0
      _warningCount = 0
      fatalError("There were errors.")
    }
  }

  def isDebugEnabled(implicit section: DebugSection): Boolean = debugSections(section)

  def ifDebug(pos: Position, body: (Any => Unit) => Any)(implicit section: DebugSection) = {
    if (isDebugEnabled) {
      body( { (msg: Any) => emit(account(Message(DEBUG(section), pos, msg))) } )
    }
  }

  def whenDebug(pos: Position, section: DebugSection)(body: (Any => Unit) => Any) {
    if (isDebugEnabled(section)) {
      body( { (msg: Any) => emit(account(Message(DEBUG(section), pos, msg))) } )
    }
  }

  def debug(pos: Position, msg: => Any)(implicit section: DebugSection): Unit = {
    ifDebug(pos, debug =>
      debug(msg)
    )(section)
  }

  // No-position alternatives
  final def info(msg: Any): Unit          = info(NoPosition, msg)
  final def warning(msg: Any): Unit       = warning(NoPosition, msg)
  final def error(msg: Any): Unit         = error(NoPosition, msg)
  final def error(e: Throwable): Unit     = logTrace(ERROR, e)
  final def title(msg: Any): Unit         = title(NoPosition, msg)
  final def fatalError(msg: Any): Nothing = fatalError(NoPosition, msg)

  final def internalError(msg: Any): Nothing = internalError(NoPosition, msg)
  final def internalError(e: Throwable): Nothing = {
    logTrace(INTERNAL, e)
    internalError(e.getMessage)
  }

  final def internalAssertion(cond : Boolean, msg: Any) : Unit = internalAssertion(cond,NoPosition, msg)

  final def debug(msg: => Any)(implicit section: DebugSection): Unit = debug(NoPosition, msg)
  final def ifDebug(body: (Any => Unit) => Any)(implicit section: DebugSection): Unit =
    ifDebug(NoPosition, body)
  final def whenDebug(section: DebugSection)(body: (Any => Unit) => Any): Unit =
    whenDebug(NoPosition, section)(body)

  final def debug(pos: Position, msg: => Any, e: Throwable)(implicit section: DebugSection): Unit = {
    if (isDebugEnabled(section)) {
      debug(pos, msg)
      logTrace(DEBUG(section), e)
    }
  }

  final def debug(e: Throwable)(implicit section: DebugSection): Unit =
    debug(NoPosition, e.getMessage, e)

  private def logTrace(severity: Severity, e: Throwable): Unit = synchronized {
    var indent = 0
    def log(msg: Any) = emit(account(Message(severity, NoPosition, ("  " * indent) + msg)))

    var ex = e
    while (ex != null) {
      val prefix = if (indent == 0) "Error" else "Cause"
      log(s"$prefix: ${ex.getMessage}. Trace:")
      for (frame <- ex.getStackTrace)
        log(s"- $frame")

      indent += 1

      val cause = e.getCause
      ex = if (cause ne ex) cause else null // don't loop forever on the same cause!
    }
  }

}

class DefaultReporter(debugSections: Set[DebugSection]) extends Reporter(debugSections) {
  protected def severityToPrefix(sev: Severity): String = sev match {
    case ERROR    => "["+Console.RED              +" Error  "+Console.RESET+"]"
    case WARNING  => "["+Console.YELLOW           +"Warning "+Console.RESET+"]"
    case INFO     => "["+Console.BLUE             +"  Info  "+Console.RESET+"]"
    case FATAL    => "["+Console.RED+Console.BOLD +" Fatal  "+Console.RESET+"]"
    case INTERNAL => "["+            Console.BOLD +"Internal"+Console.RESET+"]"
    case DEBUG(_) => "["+Console.MAGENTA          +" Debug  "+Console.RESET+"]"
  }

  def smartPos(p: Position): String = {
    if (p == NoPosition) {
      ""
    } else {
      val target = p.file.getAbsolutePath()
      val here   = new java.io.File(".").getAbsolutePath().stripSuffix(".")
      val diff   = target.stripPrefix(here)

      val filePos = diff+":"

      filePos + p + ": "
    }
  }

  def emit(msg: Message) = synchronized {
    println(reline(severityToPrefix(msg.severity), smartPos(msg.position) + msg.msg.toString))
    printLineContent(msg.position)
  }

  def getLine(pos: Position): Option[String] = {
    val lines =
      if (pos == NoPosition) Nil
      else scala.io.Source.fromFile(pos.file).getLines.toList

    if (lines.size >= pos.line && pos.line > 0) {
      Some(lines(pos.line - 1))
    } else {
      None
    }
  }

  val prefixSize = 11

  val blankPrefix = " " * prefixSize

  def printLineContent(pos: Position): Unit = {
    getLine(pos) match {
      case Some(line) =>
        println(blankPrefix+line)
        pos match {
          case rp: RangePosition =>
            val bp = rp.focusBegin
            val ep = rp.focusEnd

            val carret = if (bp.line == ep.line) {
              val width = Math.max(ep.col - bp.col, 1)
              "^" * width
            } else {
              val width = Math.max(line.length+1-bp.col, 1)
              ("^" * width)+"..."
            }

            println(blankPrefix+(" " * (bp.col - 1) + Console.RED+carret+Console.RESET))

          case op: OffsetPosition =>
            println(blankPrefix+(" " * (op.col - 1) + Console.RED+"^"+Console.RESET))
        }
      case None =>
    }
  }

  protected def reline(pfx: String, msg: String) : String = {
    pfx+" "+msg.replaceAll("\n", s"\n$pfx ")
  }

}

class PlainTextReporter(debugSections: Set[DebugSection]) extends DefaultReporter(debugSections) {
  override protected def severityToPrefix(sev: Severity): String = sev match {
    case ERROR    => "[ Error  ]"
    case WARNING  => "[Warning ]"
    case INFO     => "[  Info  ]"
    case FATAL    => "[ Fatal  ]"
    case INTERNAL => "[Internal]"
    case DEBUG(_) => "[ Debug  ]"
  }
}

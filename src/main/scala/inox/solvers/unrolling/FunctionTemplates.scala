/* Copyright 2009-2016 EPFL, Lausanne */

package inox
package solvers
package unrolling

import utils._

import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}

trait FunctionTemplates { self: Templates =>
  import context._
  import program._
  import program.trees._
  import program.symbols._

  import functionsManager._

  object FunctionTemplate {
    private val cache: MutableMap[TypedFunDef, FunctionTemplate] = MutableMap.empty

    def apply(tfd: TypedFunDef): FunctionTemplate = cache.getOrElseUpdate(tfd, {
      val body: Expr = timers.solvers.simplify.run { simplifyFormula(tfd.fullBody) }

      val fdArgs: Seq[Variable] = tfd.params.map(_.toVariable)
      val call: Expr = tfd.applied(fdArgs)

      val start = Variable.fresh("start", BooleanType(), true)
      val pathVar = start -> encodeSymbol(start)
      val arguments = fdArgs.map(v => v -> encodeSymbol(v))
      val substMap = arguments.toMap + pathVar

      val tmplClauses = {
        val (p, cls) = mkExprClauses(start, body, substMap)
        cls + (start -> Equals(call, p))
      }

      val (contents, str) = Template.contents(
        pathVar, arguments, tmplClauses, optCall = Some(tfd))

      val funString : () => String = () => {
        "Template for def " + tfd.signature +
        "(" + tfd.params.map(a => a.id + " : " + a.getType).mkString(", ") + ") : " +
        tfd.returnType + " is :\n" + str()
      }

      new FunctionTemplate(contents, funString)
    })
  }

  class FunctionTemplate private(
    val contents: TemplateContents,
    stringRepr: () => String) extends Template {

    private lazy val str : String = stringRepr()
    override def toString : String = str
  }

  def instantiateCall(blocker: Encoded, fi: Call): Clauses = {
    val gen = nextGeneration(currentGeneration)
    val notBlocker = mkNot(blocker)

    callInfos.get(blocker) match {
      case Some((exGen, origGen, _, exFis)) =>
        // PS: when recycling `b`s, this assertion becomes dangerous.
        // It's better to simply take the max of the generations.
        // assert(exGen == gen, "Mixing the same id "+id+" with various generations "+ exGen+" and "+gen)

        val minGen = gen min exGen

        callInfos += blocker -> (minGen, origGen, notBlocker, exFis + fi)
      case None =>
        callInfos += blocker -> (gen, gen, notBlocker, Set(fi))
    }

    Seq.empty
  }

  def getCalls: Seq[(Encoded, Call)] = defBlockers.toList.map(p => p._2 -> p._1)

  private val callCache: MutableMap[TypedFunDef, (Seq[Encoded], Encoded)] = MutableMap.empty
  private[unrolling] def mkCall(tfd: TypedFunDef, args: Seq[Encoded]): Encoded = {
    val (asT, call) = callCache.getOrElseUpdate(tfd, {
      val as = tfd.params.map(vd => Variable.fresh("x", vd.tpe, true))
      val asT = as.map(encodeSymbol)
      (asT, mkEncoder((as zip asT).toMap)(tfd.applied(as)))
    })

    mkSubstituter((asT zip args).toMap)(call)
  }

  private[unrolling] object functionsManager extends Manager {
    // Function instantiations have their own defblocker
    private[FunctionTemplates] val defBlockers = new IncrementalMap[Call, Encoded]()

    // Keep which function invocation is guarded by which guard,
    // also specify the generation of the blocker.
    private[FunctionTemplates] val callInfos   = new IncrementalMap[Encoded, (Int, Int, Encoded, Set[Call])]()

    val incrementals: Seq[IncrementalState] = Seq(callInfos, defBlockers)

    def unrollGeneration: Option[Int] =
      if (callInfos.isEmpty) None
      else Some(callInfos.values.map(_._1).min)

    def satisfactionAssumptions: Seq[Encoded] = callInfos.map(_._2._3).toSeq
    def refutationAssumptions: Seq[Encoded] = Seq.empty

    def promoteBlocker(b: Encoded): Boolean = {
      if (callInfos contains b) {
        val (_, origGen, notB, fis) = callInfos(b)
        callInfos += b -> (currentGeneration, origGen, notB, fis)
        true
      } else {
        false
      }
    }

    def unroll: Clauses = if (callInfos.isEmpty) Seq.empty else {
      val blockers = callInfos.filter(_._2._1 <= currentGeneration).toSeq.map(_._1)

      val newClauses = new scala.collection.mutable.ListBuffer[Encoded]

      val thisCallInfos = blockers.flatMap(id => callInfos.get(id).map(id -> _))
      callInfos --= blockers

      val remainingBlockers = MutableSet.empty ++ blockers

      for ((blocker, (gen, _, _, calls)) <- thisCallInfos if calls.nonEmpty && !abort && !pause;
           _ = remainingBlockers -= blocker;
           call @ Call(tfd, args) <- calls if !abort) {
        val newCls = new scala.collection.mutable.ListBuffer[Encoded]

        val defBlocker = defBlockers.get(call) match {
          case Some(defBlocker) =>
            // we already have defBlocker => f(args) = body
            defBlocker

          case None =>
            // we need to define this defBlocker and link it to definition
            val defBlocker = encodeSymbol(Variable.fresh("d", BooleanType(), true))

            // we generate helper equality clauses that stem from purity
            for ((pcall, pblocker) <- defBlockers if pcall.tfd == tfd) {
              def makeEq(tpe: Type, e1: Encoded, e2: Encoded): Encoded =
                if (!unrollEquality(tpe)) mkEquals(e1, e2) else {
                  mkApp(equalitySymbol(tpe)._2, FunctionType(Seq(tpe, tpe), BooleanType()), Seq(e1, e2))
                }

              if (tfd.params.exists(vd => unrollEquality(vd.tpe)) || unrollEquality(tfd.returnType)) {
                val argPairs = (pcall.args zip args)
                val cond = mkAnd((tfd.params.map(_.tpe) zip argPairs).map {
                  case (tpe, (e1, e2)) => makeEq(tpe, e1.encoded, e2.encoded)
                } : _*)
                val entail = makeEq(tfd.returnType,
                  mkCall(tfd, pcall.args.map(_.encoded)),
                  mkCall(tfd, args.map(_.encoded)))
                newClauses += mkImplies(mkAnd(pblocker, defBlocker, cond), entail)
              }
            }

            defBlockers += call -> defBlocker

            newCls ++= FunctionTemplate(tfd).instantiate(defBlocker, args)
            defBlocker
        }

        // We connect it to the defBlocker: blocker => defBlocker
        if (defBlocker != blocker) {
          registerImplication(blocker, defBlocker)
          newCls += mkImplies(blocker, defBlocker)
        }

        reporter.debug("Unrolling behind "+call+" ("+newCls.size+")")
        for (cl <- newCls) {
          reporter.debug("  . "+cl)
        }

        newClauses ++= newCls
      }

      for ((b, (gen, origGen, notB, calls)) <- thisCallInfos if remainingBlockers(b)) callInfos.get(b) match {
        case Some((newGen, _, _, newCalls)) => callInfos += b -> (gen min newGen, origGen, notB, calls ++ newCalls)
        case None => callInfos += b -> (gen, origGen, notB, calls)
      }

      reporter.debug(s"   - ${newClauses.size} new clauses")

      newClauses.toSeq
    }
  }
}

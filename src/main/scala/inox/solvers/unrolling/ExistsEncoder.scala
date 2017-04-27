/* Copyright 2009-2017 EPFL, Lausanne */

package inox
package solvers
package unrolling

trait ExistsEncoder extends ast.ProgramEncoder {

  protected object encoder extends ast.TreeTransformer {
    val s: sourceProgram.trees.type = sourceProgram.trees
    val t: targetProgram.trees.type = targetProgram.trees

    override def transform(e: s.Expr): t.Expr = e match {
      case s.Exists(args, body) => t.Not(t.Forall(args.map(transform(_)), t.Not(transform(body))))
      case _ => super.transform(e)
    }
  }

  protected object decoder extends ast.TreeTransformer {
    val s: targetProgram.trees.type = targetProgram.trees
    val t: sourceProgram.trees.type = sourceProgram.trees
  }
}
package com.ffn

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox

@compileTimeOnly("enable -Ymacro-annotations to expand macro annotations")
class withNamedToString extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro withNamedToStringAnnotation.impl
}

object withNamedToStringAnnotation {

  /** Implements toString with  BetterToString.forThisClass for this case class */
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe.*

    val result = annottees.map(_.tree) match {
      case q"$mods class $className[..$tparams](..$params) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: companion =>
        if (!mods.asInstanceOf[Modifiers].hasFlag(Flag.CASE)) {
          c.abort(c.enclosingPosition, "@withNamedToString can only be applied to case classes")
        }

        val hasToString = body.exists {
          case DefDef(_, TermName("toString"), _, _, _, _) => true
          case _                                           => false
        }

        if (hasToString) {
          c.abort(
            c.enclosingPosition,
            s"Case class $className already defines toString. Remove the @withNamedToString annotation or the toString method."
          )
        }

        val toStringMethod =
          q"""
          override def toString: String = com.ffn.NamedToString.forThisClass
        """
        // macro annotations work by writing both the type and the companion definition all over again with additional members (toString in this case)
        val newClassDef = q"""
          $mods class $className[..$tparams](..$params) extends { ..$earlydefns } with ..$parents { 
            $self => 
            ..$body
            $toStringMethod
          }
        """

        companion match {
          case Nil =>
            c.Expr[Any](q"$newClassDef")
          case List(companionDef) =>
            c.Expr[Any](q"$newClassDef; $companionDef")
          case _ =>
            c.abort(c.enclosingPosition, "Weird class/companion composition?")
        }

      case _ =>
        c.abort(c.enclosingPosition, "@withNamedToString can only be applied to case classes")
    }

    result
  }
}

package com.ffn

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

object NamedToString {

  case class hidden() extends StaticAnnotation

  /** Generates `toString` similar to regular case classes `toString`, but including parameter names */
  def forThisClass: String = macro betterToStringImpl

  def betterToStringImpl(c: blackbox.Context): c.Expr[String] = {
    import c.universe.*

    val enclosingClass = c.internal.enclosingOwner.owner.asClass

    val className = enclosingClass.name.decodedName.toString

    val params = enclosingClass.primaryConstructor.asMethod.paramLists.flatten

    if (params.isEmpty) {
      val result =
        q"""
          $className + "()"
         """
      c.Expr[String](result)
    } else {
      // "foo" + "=" + this.foo.toString + "," ...
      val combined = params
        .filter(!_.annotations.map(_.toString).contains[String]("com.ffn.NamedToString.hidden"))
        .map { param =>
          val paramName = param.name.decodedName.toString

          val fieldTerm = TermName(paramName)
          q"""
           $paramName + "=" + String.valueOf(this.$fieldTerm)
           """
        }
        .reduce[Tree] { (acc, field) =>
          q"""
           $acc + ", " + $field
           """
        }

      val result =
        q"""
            $className + "(" + $combined + ")"
          """
      c.Expr[String](result)
    }
  }
}

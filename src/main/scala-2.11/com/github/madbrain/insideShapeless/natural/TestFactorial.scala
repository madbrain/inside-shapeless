package com.github.madbrain.insideShapeless.natural

import shapeless.ops.nat.{ToInt, Prod}
import shapeless.{Nat, Succ}

object TestFactorial {

  trait Factorial[N <: Nat, Out <: Nat]

  object Factorial {

    implicit val fact0 = new Factorial[Nat._0, Nat._1] { }

    implicit def factN[N <: Nat, X <: Nat, R <: Nat](implicit fact: Factorial[N, X], mult: Prod.Aux[Succ[N], X, R]) = new Factorial[Succ[N], R] { }

    def apply[N <: Nat, R <: Nat](x: N)(implicit fact: Factorial[N, R], toInt: ToInt[R]) = toInt()
  }

  def main(args: Array[String]) {
    println(Factorial(Nat._4))
  }
}

package com.github.madbrain.insideShapeless.natural

import shapeless.ops.nat.{Sum, ToInt}
import shapeless.{Nat, Succ}

object TestFibonacci {

  trait Fibonacci[N <: Nat, Out <: Nat]

  object Fibonacci {

    implicit val fibo1 = new Fibonacci[Nat._1, Nat._1] { }

    implicit val fibo2 = new Fibonacci[Nat._2, Nat._2] { }

    implicit def fiboN[N <: Nat, A <: Nat, B <: Nat, R <: Nat](implicit fN1: Fibonacci[N, A],
                                                               fN2: Fibonacci[Succ[N], B],
                                                               mult: Sum.Aux[A, B, R]) = new Fibonacci[Succ[Succ[N]], R] { }

    def apply[N <: Nat, R <: Nat](x: N)(implicit fibo: Fibonacci[N, R], toInt: ToInt[R]) = toInt()
  }

  def main(args: Array[String]) {
    println(Fibonacci(Nat._6))
  }


}

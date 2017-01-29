package com.github.madbrain.insideShapeless.natural

import shapeless.Nat
import shapeless.ops.nat.{Sum, ToInt}

class NatMatrix[N <: Nat, M <: Nat](implicit val toIntN: ToInt[N], toIntM: ToInt[M]) {
  val elements = Array.ofDim[Double](toIntN(), toIntM())

  def add(m: NatMatrix[N, M]): NatMatrix[N, M] = {
    val r = new NatMatrix[N, M]
    0.until(toIntN()).foreach(i =>
      0.until(toIntM()).foreach(j =>
        r.elements(i)(j) = elements(i)(j) + m.elements(i)(j)
      ))
    r
  }

  def mul[R <: Nat](m: NatMatrix[M, R])(implicit toIntR: ToInt[R]): NatMatrix[R, N] = {
    val r = new NatMatrix[R, N]
    0.until(toIntR()).foreach(i => {
      0.until(toIntN()).foreach(j => {
        var sum = 0.0
        0.until(toIntM()).foreach(k => {
          sum += elements(j)(k) + m.elements(k)(i)
        })
        r.elements(i)(j) = sum
      })
    })
    r
  }

  def concat[R <: Nat, T <: Nat](m: NatMatrix[R, M])(implicit auxSum: Sum.Aux[N, R, T]): NatMatrix[T, M] = ???

}

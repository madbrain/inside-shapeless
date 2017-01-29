package com.github.madbrain.insideShapeless.natural

class Matrix(val n: Int, val m: Int) {
  val elements = Array.ofDim[Double](n, m)

  def add(o: Matrix): Matrix = {
    if (o.n != n || o.m != m) {
      throw new RuntimeException("incompatible size")
    }
    val r = new Matrix(n, m)
    0.until(n).foreach(i =>
      0.until(m).foreach(j =>
        r.elements(i)(j) = elements(i)(j) + o.elements(i)(j)
      ))
    r
  }

  def mul(o: Matrix): Matrix = {
    if (m != o.n) {
      throw new RuntimeException("incompatible size")
    }
    val r = new Matrix(n, o.m)
    0.until(n).foreach(i => {
      0.until(o.m).foreach(j => {
        var sum = 0.0
        0.until(m).foreach(k => {
          sum += elements(i)(k) + o.elements(k)(j)
        })
        r.elements(i)(j) = sum
      })
    })
    r
  }

  def concat(o: Matrix): Matrix = {
    if (n != o.n) {
      throw new RuntimeException("incompatible size")
    }
    val r = new Matrix(n, m + o.m)
    0.until(n).foreach(i => {
      0.until(m).foreach(j => {
        r.elements(i)(j) = elements(i)(j)
      })
      0.until(o.m).foreach(j => {
        r.elements(i)(m + j) = o.elements(i)(j)
      })
    })
    r
  }

}

object TestMatrix {

  def main(args: Array[String]) {
    val x = new Matrix(6, 5)
    val y = new Matrix(5, 6)
    val z = new Matrix(6, 4)

    println(x.mul(y))
    println(x.concat(z))

//    val x = new NatMatrix[_6, _5]
//    val y = new NatMatrix[_5, _6]
//    val z = new NatMatrix[_4, _5]
//
//    println(x.mul(y))
//    println(x.concat(z))

  }

}

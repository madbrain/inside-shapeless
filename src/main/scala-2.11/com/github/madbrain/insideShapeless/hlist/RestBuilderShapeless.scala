package com.github.madbrain.insideShapeless.hlist

import shapeless._
import shapeless.nat._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{Length, ZipApply}
import shapeless.ops.traversable.FromTraversable

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

object RestBuilderShapeless {
  // voir le blog sur le DSL rest
  // http://codetunes.com/2012/scala-dsl-tutorial-writing-web-framework-router/

  sealed trait PathElem
  case class Static(value: String) extends PathElem
  case object * extends PathElem

  case class Route[T](m: Method, elems: List[PathElem], func: Seq[String] => T) {
    def run(args: String*) = {
      if (args.size != elems.count(p => p == *)) {
        throw new RuntimeException("bad number of args")
      }
      func(args)
    }
  }

  case class RouteDef[R <: Nat](method: Method, elems: List[PathElem]) {
    def withMethod(m : Method): RouteDef[R] = RouteDef(m, elems)
    def /(static: Static) = RouteDef(method, elems :+ static)
    def /(p: PathElem) = RouteDef[Succ[R]](method, elems :+ p)

    def to[F, Y <: HList, Z <: HList, T, A <: HList](func : F)(implicit fp: FnToProduct.Aux[F, Y => T],
                                                               size: Length.Aux[Y, R],
                                                               proc: CreateProc.Aux[Y, Z],
                                                               zipper: ZipApply.Aux[Z, A, Y],
                                                               fromList: FromTraversable[A]
    ) = {
      val resFunc = (a : Seq[String]) => fromList(a) match {
        case Some(r) => fp(func)(zipper(proc(), r))
        case None => throw new RuntimeException
      }
      Route(method, elems, resFunc)
    }
  }

  sealed trait Method {
    def on[R <: Nat](routeDef: RouteDef[R]): RouteDef[R] = routeDef.withMethod(this)
  }
  case object ANY extends Method
  case object GET extends Method
  case object PUT extends Method
  case object POST extends Method

  implicit def stringToRouteDef(value: String) = RouteDef[_0](ANY, Static(value) :: Nil)
  implicit def asterixToRoutePath1(ast: *.type) = RouteDef[_1](ANY, ast :: Nil)

  trait PathParam[K] {
    def apply(x : K): String
    def unapply(x: String): Option[K]
  }

  implicit val StringPathParam: PathParam[String] = new PathParam[String] {
    def apply(s: String) = s
    def unapply(s: String) = Some(s)
  }

  implicit val IntPathParam: PathParam[Int] = new PathParam[Int] {
    def apply(s: Int) = s.toString
    def unapply(s: String): Option[Int] = try { Some(Integer.parseInt(s)) } catch { case e: NumberFormatException => None }
  }

  implicit val BooleanPathParam: PathParam[Boolean] = new PathParam[Boolean] {
    def apply(b: Boolean) = b.toString
    def unapply(s: String) = s.toLowerCase match {
      case "1" | "true" | "yes" => Some(true)
      case "0" | "false" | "no" => Some(false)
      case _ => None
    }
  }

  /**
    * Inspiré de FillWith
    */
  trait CreateProc[L <: HList] extends DepFn0 { type Out <: HList }

  object CreateProc {
    @implicitNotFound("Vérifier les instances de PathParam implicites")
    type Aux[L <: HList, Out0 <: HList] = CreateProc[L] { type Out = Out0 }

    implicit def hnilFill[F]: Aux[HNil, HNil] = new CreateProc[HNil] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def hconsFill[Head, Tail <: HList](implicit pp: PathParam[Head],
                                                mt: CreateProc[Tail],
                                                m : ClassTag[Head]): Aux[Head :: Tail, (String => Head) :: mt.Out] =
      new CreateProc[Head :: Tail] {
        type Out = (String => Head) :: mt.Out
        def apply(): Out = (hc _) :: mt()
        def hc(x: String) = pp.unapply(x) match {
          case Some(r) => r
          case None => throw new RuntimeException(s"Cannot convert '$x' to ${m.runtimeClass.getName}")
        }
      }
  }

}

object ApplicationShapeless {
  import RestBuilderShapeless._

  val fooRoute = GET on "foo" to ApplicationShapeless.foo _
  val showRoute = PUT on "show" / * to ApplicationShapeless.show _
  val barRoute = POST on "bar" / * / * to ApplicationShapeless.bar _

  def foo() = {
    println("foo")
  }
  def show(a: Int) = {
    println(s"show $a")
  }
  def bar(a: String, b: Boolean) = {
    println(s"bar $a $b")
  }

  def main(args: Array[String]) {
    showRoute.run("10")
  }

}

package com.github.madbrain.insideShapeless.hlist

object RestBuilder {
  // voir le blog sur le DSL rest
  // http://codetunes.com/2012/scala-dsl-tutorial-writing-web-framework-router/

  sealed trait Method {
    def on[R](routeDef: RouteDef[R]): R = routeDef.withMethod(this)
  }
  case object ANY extends Method
  case object GET extends Method
  case object PUT extends Method
  case object POST extends Method

  sealed trait PathElem
  case class Static(value: String) extends PathElem
  case object * extends PathElem

  sealed trait RouteDef[R] {
    def withMethod(m : Method): R
  }
  case class RouteDef0(method: Method, elems: List[PathElem]) extends RouteDef[RouteDef0] {
    override def withMethod(m: Method): RouteDef0 = RouteDef0(m, elems)
    def /(static: Static) = RouteDef0(method, elems :+ static)
    def /(p: PathElem) = RouteDef1(method, elems :+ p)
    def to(f0: () => Out) = Route0(this, f0)
  }
  case class RouteDef1(method: Method, elems: List[PathElem]) extends RouteDef[RouteDef1] {
    override def withMethod(m: Method): RouteDef1 = RouteDef1(m, elems)
    def /(static: Static) = RouteDef0(method, elems :+ static)
    def /(p: PathElem) = RouteDef2(method, elems :+ p)
    def to[A : PathParam](f1: (A) => Out) = Route1(this, f1)
  }
  case class RouteDef2(method: Method, elems: List[PathElem]) extends RouteDef[RouteDef2] {
    override def withMethod(m: Method): RouteDef2 = RouteDef2(m, elems)
    def /(static: Static) = RouteDef2(method, elems :+ static)
    def to[A : PathParam, B : PathParam](f2: (A, B) => Out) = Route2(this, f2)
  }

  implicit def stringToRouteDef(value: String) = RouteDef0(ANY, Static(value) :: Nil)
  implicit def asterixToRoutePath1(ast: *.type) = RouteDef1(ANY, ast :: Nil)

  type Out = Unit
  sealed trait Route[RD] {
    def run(args: String*): Option[Out]
  }
  case class Route0(rd: RouteDef0, f0: () => Out) extends Route[RouteDef0] {
    override def run(args: String*) = Some(f0())
  }
  case class Route1[A](rd: RouteDef1, f1: (A) => Out)(implicit ta: PathParam[A]) extends Route[RouteDef1] {
    override def run(args: String*) = ta.unapply(args(0)).map(f1)
  }
  case class Route2[A, B](rd: RouteDef2, f2: (A, B) => Out)(
      implicit ta: PathParam[A], tb: PathParam[B]) extends Route[RouteDef2] {
    override def run(args: String*) = (ta.unapply(args(0)), tb.unapply(args(1))) match {
      case (Some(a), Some(b)) => Some(f2(a, b))
      case _ => None
    }
  }

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

}

object Application {
  import RestBuilder._

  val fooRoute = GET on "foo" to ApplicationShapeless.foo
  val showRoute = PUT on "show" / * to ApplicationShapeless.show
  val barRoute = POST on "bar" / * / * to ApplicationShapeless.bar

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

package com.github.madbrain.insideShapeless.hlist

import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Prepend
import shapeless.ops.traversable.FromTraversable
import shapeless.{::, HList, HNil}

object Ast {
  case  class FooFileNode(statements: Seq[Statement])
  trait Statement
  trait Expression
  case class Declaration(name: String, expr: Expression) extends Statement
  case class GreaterNode(left: Expression, right: Expression) extends Expression
  case class SubtractNode(left: Expression, right: Expression) extends Expression
  case class MultiplyNode(left: Expression, right: Expression) extends Expression
  case class IdentNode(name: String) extends Expression
  case class IntegerNode(name: String) extends Expression
  case class StringNode(name: String) extends Expression
  case class ArgumentsNode(elements: Seq[Expression])
  case class FunctionCall(name: String, args: ArgumentsNode) extends Expression
}

// ======================

case class Token(value: String)

trait Element
case class NonTerminal[T]() extends Element
case class Terminal(kind: Int) extends Element
case class ValuedTerminal(kind: Int) extends Element

case class Rule[T](lhs: NonTerminal[T], rhs: Seq[Element], func: Seq[Any] => T)

class RuleBuilder[T](lhs: NonTerminal[T]) {
  def ::= [X <: T](rb: BuiltRhs[X]): Rule[T] = Rule(lhs, rb.elements, rb.func)

  def ::= [X <: T, H <: HList](rb: RhsBuilder[X :: HNil]): Rule[T] = {
    val f: Seq[Any] => T = x => x.head.asInstanceOf[T]
    Rule(lhs, rb.elements, f)
  }
}

class BuiltRhs[R](val elements: Seq[Element], val func: Seq[Any] => R)

class RhsBuilder[H <: HList](val elements: Seq[Element]) {
  def ~ [T, R <: HList] (nt: NonTerminal[T])(
    implicit prepend : Prepend.Aux[H, T :: HNil, R]): RhsBuilder[R] = new RhsBuilder(elements :+ nt)

  def ~ [R <: HList] (t: ValuedTerminal)(
    implicit prepend : Prepend.Aux[H, Token :: HNil, R]): RhsBuilder[R] = new RhsBuilder(elements :+ t)

  def ~ (t: Terminal) = new RhsBuilder[H](elements :+ t)

  def >> [F, R] (func: F)(implicit fp: FnToProduct.Aux[F, H => R],
                          fromList: FromTraversable[H]) = {
    val newFunc = (args: Seq[Any]) => {
      fromList(args) match {
        case Some(values) => fp(func)(values)
        case None => throw new RuntimeException("burp")
      }
    }
    new BuiltRhs[R](elements, newFunc)
  }
}

object BuilderOps {
  implicit def nonTerminalToBuilder[T](nt: NonTerminal[T]) = new RhsBuilder[T :: HNil](Seq(nt))
  implicit def terminalToBuilder(t: Terminal) = new RhsBuilder[HNil](Seq(t))
  implicit def valueToBuilder(t: ValuedTerminal) = new RhsBuilder[Token :: HNil](Seq(t))

  implicit def nonTerminalToRule[T](nt: NonTerminal[T]) = new RuleBuilder(nt)

  object ø extends RhsBuilder[HNil](Seq())
}

class GrammarBuilder {

  import Ast._
  import BuilderOps._

  val IDENT = ValuedTerminal(0)
  val INTEGER = ValuedTerminal(0)
  val STRING = ValuedTerminal(0)

  val PLUS  = Terminal(1)
  val VAR = Terminal(2)
  val EQUALS = Terminal(3)
  val GREATER = Terminal(4)
  val MINUS = Terminal(5)
  val TIMES = Terminal(6)
  val LPAREN = Terminal(6)
  val RPAREN = Terminal(6)
  val COMA = Terminal(6)

  val FooFile = NonTerminal[FooFileNode]()
  val Statement = NonTerminal[Statement]()
  val StatementList = NonTerminal[Seq[Statement]]()
  val Expression = NonTerminal[Expression]()
  val ExpressionList = NonTerminal[Seq[Expression]]()
  val ExpressionListOpt = NonTerminal[Seq[Expression]]()
  val Subtract = NonTerminal[Expression]()
  val Multiply = NonTerminal[Expression]()
  val Unitary = NonTerminal[Expression]()
  val Arguments = NonTerminal[ArgumentsNode]()

  def empty[T]() = Seq[T]()
  def append[T](l: Seq[T], e: T) = l :+ e

  FooFile ::= StatementList >> FooFileNode
  StatementList ::= ø >> empty[Statement] _
  StatementList ::= StatementList ~ Statement >> append[Statement] _
  Statement ::= VAR ~ IDENT ~ EQUALS ~ Expression >> ( (t: Token, e: Expression) => Declaration(t.value, e))
  Expression ::= Subtract
  Expression ::= Subtract ~ GREATER ~ Subtract >> GreaterNode
  Subtract ::= Multiply
  Subtract ::= Subtract ~ MINUS ~ Multiply >> SubtractNode
  Multiply ::= Unitary
  Multiply ::= Multiply ~ TIMES ~ Unitary >> MultiplyNode
  Unitary ::= IDENT >> ( (t: Token) => IdentNode(t.value))
  Unitary ::= IDENT ~ Arguments >> ( (t: Token, args: ArgumentsNode) => FunctionCall(t.value, args))
  Unitary ::= INTEGER >> ( (t: Token) => IntegerNode(t.value))
  Unitary ::= STRING >> ( (t: Token) => StringNode(t.value))
  Arguments ::= LPAREN ~ ExpressionListOpt ~ RPAREN >> ArgumentsNode
  ExpressionListOpt ::= ø >> empty[Expression] _
  ExpressionListOpt ::= ExpressionList >> ( (e: Seq[Expression]) => e )
  ExpressionList ::= Expression >> ( (e: Expression) => Seq[Expression](e) )
  ExpressionList ::= ExpressionList ~ COMA ~ Expression >> append[Expression] _
}

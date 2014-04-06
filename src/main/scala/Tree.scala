package kuba.sicp

import scala.annotation.tailrec
import scala.language.implicitConversions
import Numeric._

// ex 2.27, 2.28, 2.30, 2.31
abstract class Tree[A] {
  def countLeaves: Int
  def deepReverse: Tree[A]
  def map[B](fn: A => B): Tree[B]
}
case class Branch[A](nodes: Tree[A]*) extends Tree[A] {
  def countLeaves = nodes.foldLeft(0)(_ + _.countLeaves)
  def deepReverse = Branch[A](nodes.reverse.map(_.deepReverse):_*)
  def map[B](fn: A => B) = Branch[B](nodes.toList.map(x => x.map[B](fn)):_*)
}
case class Leaf[A](value: A) extends Tree[A] {
  def countLeaves = 1
  def deepReverse = this
  def map[B](fn: A => B) = Leaf[B](fn(value))
}

object Tree {
  def fringe[A](tree: Tree[A]): Tree[A] = tree match {
    case Leaf(x) => Leaf(x)
    case Branch(nodes @ _*) => {
      def go(nodes: List[Tree[A]], acc: List[Tree[A]]): List[Tree[A]] = nodes match {
        case Nil => acc
        case Leaf(x) :: rest => go(rest, acc :+ Leaf(x))
        case Branch(nodes @ _*) :: rest => go(rest, (go(nodes.toList, acc)))
        case _ :: rest => go(rest, acc)
      }
      Branch(go(nodes.toList, Nil):_*)
    }
  }
  def scaleTree[A](tree: Tree[A], factor: A)(implicit num: Numeric[A]): Tree[A] = 
    tree.map(value => num.times(factor, value))
  def squareTree[A](tree: Tree[A])(implicit num: Numeric[A]): Tree[A] =
    tree.map(value => num.times(value, value))
}
package kuba.sicp

import scala.annotation.tailrec
import scala.language.implicitConversions

// ex 2.27, 2.28
abstract class Tree[A] {
  def countLeaves: Int
  def deepReverse: Tree[A]
}
case class Branch[A](nodes: Tree[A]*) extends Tree[A] {
  def countLeaves = nodes.foldLeft(0)(_ + _.countLeaves)
  def deepReverse = Branch[A](nodes.reverse.map(_.deepReverse):_*)
}
case class Leaf[A](value: A) extends Tree[A] {
  def countLeaves = 1
  def deepReverse = this
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
}
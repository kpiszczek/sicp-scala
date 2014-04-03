package kuba.sicp

import scala.language.implicitConversions

// ex 2.27
abstract class Tree[A] {
  def countLeaves: Int
  def deepReverse: Tree[A]
}
case class Branch[A](nodes: Tree[A]*) extends Tree[A] {
  def countLeaves = nodes.foldLeft(0)(_ + _.countLeaves)
  def deepReverse = Branch[A](nodes.toList.reverse.map(_.deepReverse):_*)
}
case class Leaf[A](value: A) extends Tree[A] {
  def countLeaves = 1
  def deepReverse = this
}
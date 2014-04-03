package kuba.sicp

import scala.language.implicitConversions

abstract class Tree[A] {
  def countLeaves: Int
}
case class Branch[A](nodes: Tree[A]*) extends Tree[A] {
  def countLeaves = nodes.foldLeft(0)(_ + _.countLeaves)
}
case class Leaf[A](value: A) extends Tree[A] {
  def countLeaves = 1
}
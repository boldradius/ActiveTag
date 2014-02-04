package activetag.dom

import activetag.dom.structure.NodeStructure

/**
 * Created by ppremont on 2/1/2014.
 */
sealed trait Node[+A] {
  def map[B](f: A => B) : Node[B] = ActionNode(f, this)
  def structure : NodeStructure
}

case class ActionNode[+A, B](f: B => A, child: Node[B]) extends Node[A] {
  def structure = child.structure
}

sealed trait Element[+A] extends Node[A]


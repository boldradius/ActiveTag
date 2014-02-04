package activetag.dom.structure

import Change.Changes
import java.io.OutputStream

/**
 * Created by ppremont on 2/1/2014.
 */

/** Instructions to build a NodeStructure, from some known Option[NodeStructure] */
sealed trait Change
case class NewElement(name: String) extends Change
case class NewText(name: String) extends Change
case class SetAttribute(name: String, value: String) extends Change
case class RemoveAttribute(name: String) extends Change
case class ReplaceChildren(childChanges: List[Changes]) extends Change


object Change {
  type Changes = List[Change]

  def changes(from: Option[NodeStructure], to: NodeStructure) : Changes = to match {
    case TextStructure(t) => from match {
      case Some(TextStructure(`t`)) => Nil
      case _ => List(NewText(t))
    }
    case ElementStructure(n, at, c) => from match {
      case Some(ElementStructure(`n`, at0, c0)) => attributeChanges(at0, at) ++ replaceChildren(childChanges(c0, c))
      case _ => NewElement(n) :: attributeChanges(Nil, at) ++ replaceChildren(childChanges(Nil, c))
    }
  }

  def attributeChanges(from: List[(String, String)], to: List[(String, String)]) : Changes = {
    val fm = from.toMap
    lazy val tm = to.toMap
    to.flatMap{case (k, tv) => if (fm.get(k) == Some(tv)) Nil else List(SetAttribute(k, tv))} ++
      from.flatMap{case (k, _) => if (tm.contains(k)) Nil else List(RemoveAttribute(k))}
  }

  def replaceChildren(childChanges: List[Changes]) : List[ReplaceChildren] =
    if (childChanges.forall(_.isEmpty)) Nil else List(ReplaceChildren(childChanges))

  def childChanges(from: List[NodeStructure], to: List[NodeStructure]) : List[Changes] =
    from.zip(to).map{case (f, t) => changes(Some(f), t)} ++
      to.drop(from.length).map(changes(None, _))

}

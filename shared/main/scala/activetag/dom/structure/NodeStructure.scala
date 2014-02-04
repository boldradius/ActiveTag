package activetag.dom.structure

/**
 * Created by ppremont on 2/1/2014.
 */
sealed trait NodeStructure
case class TextStructure(text: String) extends NodeStructure
case class ElementStructure(name: String, attributes: List[(String, String)], childNodes: List[NodeStructure]) extends NodeStructure


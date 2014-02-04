package activetag.persist

import org.scalacheck._
import org.scalacheck.Prop._
import Persister._

/**
 * Created by ppremont on 2/4/2014.
 */
object PersistTest extends Properties("Persistence") {
  def persists[A](implicit p: Persister[A], a: Arbitrary[A]): Prop = forAll {
    (a: A) => p.read(p.write(a, Nil)) == (a, Nil)
  }
  def persists[A, B](compareAs: A => B)(implicit p: Persister[A], a: Arbitrary[A]): Prop = forAll {
    (a: A) =>
      val (a2, l2) = p.read(p.write(a, Nil))
      (compareAs(a2), l2) == (compareAs(a), Nil)
  }
  property("Unit") = persists[Unit]
  property("Byte") = persists[Byte]
  property("Boolean") = persists[Boolean]
  property("Short") = persists[Short]
  property("Char") = persists[Char]
  property("Int") = persists[Int]
  property("Long") = persists[Long]
  property("(Byte, Byte)") = persists[(Byte, Byte)]
  property("Either[Byte,Byte]") = persists[Either[Byte,Byte]]
  property("List[Char]") = persists[List[Char]]
  property("Array[Int]") = persists[Array[Int], List[Int]](_.toList)
  property("Array[Char]") = persists[Array[Char], List[Char]](_.toList)
  property("String") = persists[String]
}
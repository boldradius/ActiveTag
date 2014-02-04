package activetag.persist

/**
 * Created by ppremont on 2/3/2014.
 */

import Persister._

sealed trait Persister[A] {
  def write(a: A, l: List[Byte]) : List[Byte]
  def read(l: List[Byte]) : (A, List[Byte])
}
case class MapPersister[A, B](w: A => B, r: B => A, p: Persister[B]) extends Persister[A] {
  def write(a: A, l: List[Byte]) = p.write(w(a), l)
  def read(l : List[Byte]) = {val (b, l2) = p.read(l); (r(b), l2)}
}
case object BytePersister extends Persister[Byte] {
  def write(a: Byte, l: List[Byte]) = a :: l
  def read(l: List[Byte]) = (l.head, l.tail)
}
case object UnitPersister extends Persister[Unit] {
  def write(a: Unit, l: List[Byte]) = l
  def read(l: List[Byte]) = ({}, l)
}
case class TuplePersister[A, B](pa: Persister[A], pb: Persister[B]) extends Persister[(A, B)] {
  def write(a: (A, B), l: List[Byte]) = pa.write(a._1,pb.write(a._2, l))
  def read(l : List[Byte]) = {val (a, l2) = pa.read(l); val (b, l3) = pb.read(l2); ((a, b), l3)}
}
case class EitherPersister[A, B](pa: Persister[A], pb: Persister[B]) extends Persister[Either[A, B]] {
  def write(a: Either[A, B], l: List[Byte]) = booleanPersister.write(a.isRight, a.fold(pa.write(_, l), pb.write(_, l)))
  def read(l : List[Byte]) = {
    val (isRight, l2) = booleanPersister.read(l);
    if (isRight) {val (b, l3) = pb.read(l2); (Right(b), l3)}
    else {val (a, l3) = pa.read(l2); (Left(a), l3)}
  }
}
case class ArrayPersister[A](p: Persister[A], c: reflect.ClassTag[A]) extends Persister[Array[A]] {
  def write(a: Array[A], l: List[Byte]) = IntPersister.write(a.length, a.foldRight(l)((e, b) => p.write(e, b)))
  def read(l: List[Byte]) = {
    val (len, l2) = IntPersister.read(l)
    implicit val ct = c
    val arr = new Array[A](len)
    var i = 0
    var lout = l2
    while (i < len) {
      val (e, lout2) = p.read(lout)
      arr(i) = e
      lout = lout2
      i = i + 1
    }
    (arr, lout)
  }
}
case object LongPersister extends Persister[Long] {
  def write(a: Long, l: List[Byte]) = writeLongBytes(8, a, l)
  def read(l: List[Byte]) = readLongBytes(8, l)
}
case object IntPersister extends Persister[Int] {
  def write(a: Int, l: List[Byte]) = writeLongBytes(4, a, l)
  def read(l: List[Byte]) = {val (a, l2) = readLongBytes(4, l); (a.toInt, l2)}
}
case object ShortPersister extends Persister[Short] {
  def write(a: Short, l: List[Byte]) = writeLongBytes(2, a, l)
  def read(l: List[Byte]) = {val (a, l2) = readLongBytes(2, l); (a.toShort, l2)}
}
object Persister {
  implicit val unitPersister = UnitPersister
  implicit val bytePersister = BytePersister
  implicit val shortPersister = ShortPersister
  implicit val intPersister = IntPersister
  implicit val longPersister = LongPersister
  implicit def tuplePersister[A, B](implicit pa: Persister[A], pb: Persister[B]) = TuplePersister(pa, pb)
  implicit def eitherPersister[A, B](implicit pa: Persister[A], pb: Persister[B]) = EitherPersister(pa, pb)
  implicit def arrayPersister[A](implicit pa: Persister[A], c: reflect.ClassTag[A]) = ArrayPersister(pa, c)
  implicit val booleanPersister : Persister[Boolean] = MapPersister[Boolean, Byte](if (_) 1 else 0, _ > 0, BytePersister)
  implicit val charPersister : Persister[Char] = MapPersister[Char, Short](_.toShort, _.toChar, ShortPersister)
  implicit val stringPersister : Persister[String] = MapPersister[String, Array[Char]](_.toArray, _.mkString, implicitly[Persister[Array[Char]]])
  implicit def listPersister[A](implicit p : Persister[A], c: reflect.ClassTag[A]) : Persister[List[A]] = MapPersister[List[A], Array[A]](_.toArray, _.toList, implicitly[Persister[Array[A]]])
  implicit def writeLongBytes(n: Int, a: Long, l: List[Byte]) : List[Byte] =
    if (n <= 0) l
    else writeLongBytes(n - 1, a >>> 8, a.toByte :: l)
  implicit def readLongBytes(n: Int, l : List[Byte], a: Long = 0) : (Long, List[Byte]) =
    readLongBytesU(n - 1, l.tail, (a << 8) + (l.head:Long))
  implicit def readLongBytesU(n: Int, l : List[Byte], a: Long = 0) : (Long, List[Byte]) =
    if (n <= 0) (a, l)
    else readLongBytesU(n - 1, l.tail, (a << 8) + ((l.head:Long) & 0xff))
}


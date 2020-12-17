package com.brianpritchett.twopluslist
import scala.collection.AbstractSeq
import scala.collection.LinearSeq

/** A class for immutable linked lists of at least two elements
  */
class TwoPlusList[+A] private (h: A, b: A, t: List[A])
    extends AbstractSeq[A]
    with LinearSeq[A] {

  import TwoPlusList._

  override def splitAt(n: Int): (List[A], List[A]) = (take(n), drop(n))

  override def head: A = h

  override def tail: List[A] = b :: t

  def ++[AA >: A](l: List[AA]): TwoPlusList[AA] = concat(l)

  def concat[AA >: A](other: List[AA]): TwoPlusList[AA] =
    new TwoPlusList(h, b, t ::: other)

  def concatTPL[AA >: A](other: TwoPlusList[AA]): TwoPlusList[AA] =
    new TwoPlusList(h, b, t ::: other.toList)

  def flatMap[B](f: A => TwoPlusList[B]): TwoPlusList[B] =
    f(h) ++ f(b).toList ++ t.flatMap(f andThen (_.toList))

  def prepend[AA >: A](a: AA): TwoPlusList[AA] =
    new TwoPlusList(a, h, b :: t)

  def ::[AA >: A](a: AA): TwoPlusList[AA] = prepend(a)

  def append[AA >: A](a: AA): TwoPlusList[AA] =
    new TwoPlusList(h, b, t :+ a)

  def :::[AA >: A](other: TwoPlusList[AA]): TwoPlusList[AA] =
    other.concatTPL(this)

  override def scanLeft[B](z: B)(op: (B, A) => B): TwoPlusList[B] = {
    val hop = op(z, h)
    val bop = op(hop, b)
    TwoPlusList.fromListUnsafe(hop :: bop :: t.scanLeft(bop)(op))
  }

  /** Selects all elements except the last
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.init
    * val res0: List[Int] = List(1, 2, 3, 4)
    * }}}
    * @return list with the last element removed
    */
  override def init: List[A] = if (t.isEmpty) List(h) else h :: b :: t.init

  /** Keep elements matching the predicate
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.filter(_ > 2)
    * val res0: List[Int] = List(3, 4, 5)
    * }}}
    *
    * @param p returns true if element should be kept
    * @return list with elements selected by `p`
    */
  override def filter(p: A => Boolean): List[A] = {
    val includeHead = p(h)
    val includeBody = p(b)
    val fTail = t.filter(p)

    if (includeHead && includeBody) h :: b :: fTail
    else if (includeHead) h :: fTail
    else if (includeBody) b :: fTail
    else fTail
  }

  /** Inverse of `filter`
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.filterNot(_ > 2)
    * val res0: List[Int] = List(1, 2)
    * }}}
    *
    * @param p returns true if element should be discarded
    * @return list without elements matching `p`
    */
  override def filterNot(p: A => Boolean): List[A] = filter(p andThen (!_))

  /** Build a new list out of the first n elements of this list
    */
  override def take(n: Int): List[A] = {
    if (n < 1) List.empty
    else if (n == 1) List(h)
    else if (n == 2) List(h, b)
    else h :: b :: t.take(n - 2)
  }

  override def takeWhile(p: A => Boolean): List[A] = {
    if (!p(h)) List.empty
    else if (!p(b)) List(h)
    else h :: b :: t.takeWhile(p)
  }

  /** Build a new list by removing the first n elements of this list
    */
  override def drop(n: Int): List[A] = {
    if (n <= 0) toList
    else if (n == 1) b :: t
    else if (n == 2) t
    else t.drop(n - 2)
  }

  override def dropWhile(p: A => Boolean): List[A] = {
    if (p(h)) List.empty
    else if (p(b)) List(h)
    else h :: b :: t.dropWhile(p)
  }

  override def slice(from: Int, until: Int): List[A] = {
    if (until <= from) List.empty
    else if (from > 1) t.slice(from - 2, until - 2)
    else if (from == 1) b :: t.slice(0, until - 2)
    else if (from == 0 && until == 1) List(h)
    else h :: b :: t.slice(0, until - 2)
  }

  /** Applies f to all elements of this structure
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.map(_ + 1)
    * val res0: com.brianpritchett.twopluslist.TwoPlusList[Int] = TwoPlusList(2,3,List(4, 5, 6))
    * }}}
    * @param f
    * @return new `TwoPlusList` transformed by f
    */
  override def map[B](f: A => B): TwoPlusList[B] =
    new TwoPlusList(f(h), f(b), t.map(f))

  override def flatten[B](implicit
      asIterable: A => IterableOnce[B]
  ): List[B] = toList.flatten

  /** Builds a new `List` by applying a partial function to
    * all the elements from this `TwoPlusList` on which the function is defined
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.collect { case v if v > 2 => v }
    * val res0: List[Int] = List(3, 4, 5)
    * }}}
    * @param pf
    */
  override def collect[B](pf: PartialFunction[A, B]): List[B] = {
    val headDefined = pf.isDefinedAt(h)
    val bodyDefined = pf.isDefinedAt(b)
    val tailC = t.collect(pf)

    if (headDefined && bodyDefined) pf.apply(h) :: pf.apply(b) :: tailC
    else if (headDefined) pf.apply(h) :: tailC
    else if (bodyDefined) pf.apply(b) :: tailC
    else tailC
  }

  /** The size of this TwoPlusList
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.size
    * val res0: Int = 5
    * }}}
    * @return size of this list. Will always be >= 2
    */
  // override def size: Int = 2 + t.size

  /** 
    * @return length of this list
    */
  override def length: Int = 2 + t.length

  /** Selects the last element of the list
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.last
    * val res0: Int = 5
    * }}}
    * @return last element of this list
    */
  override def last: A = if (t.isEmpty) b else t.last

  override def zipWithIndex: TwoPlusList[(A, Int)] =
    TwoPlusList.fromListUnsafe(toList zip Range(1, size))

  override def span(p: A => Boolean): (List[A], List[A]) =
    (takeWhile(p), dropWhile(p))

  override def tapEach[U](f: A => U): TwoPlusList[A] = {
    map(f)
    this
  }

  override def iterator: Iterator[A] = toList.iterator

  override def toList: List[A] = h :: b :: t

  override def toString(): String = 
    s"TwoPlusList($h, $b, ${t.mkString(", ")})"

  override def lengthCompare(len: Int): Int = {
    if(len < 2) 1
    else t.lengthCompare(len - 2)
  }

}

object TwoPlusList {

  /** Create a `TwoPlusList` from two or more elements
    */
  def of[A](a1: A, a2: A, a3: A*): TwoPlusList[A] =
    new TwoPlusList(a1, a2, a3.toList)

  /** Create a `TwoPlusList` from two elements
    */
  def two[A](a1: A, a2: A): TwoPlusList[A] = of(a1, a2)

  /** Create a `TwoPlusList` from a `List`.
    *
    * The result will be `None` if the input list size is less than 2
    *  and `Some` wrapping a`TwoPlusList` otherwise.
    *
    * @see [[fromListUnsafe]] for an unsafe version that throws an exception if
    * the input list size is less than 2.
    */
  def fromList[A](ls: List[A]): Option[TwoPlusList[A]] = ls match {
    case head :: body :: tail => Some(new TwoPlusList[A](head, body, tail))
    case _                    => None
  }

  /** Create a `TwoPlusList` from a `List`, or throw an
    * `IllegalArgumentException` if the input list is empty.
    *
    * @see [[fromList]] for a safe version that returns `None` if the input list
    * is empty.
    */
  def fromListUnsafe[A](ls: List[A]) = fromList(ls).getOrElse(
    throw new IllegalArgumentException(
      s"Cannot create a TwoPlusList from a list of size ${ls.size}"
    )
  )
}

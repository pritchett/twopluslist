package com.brianpritchett.twopluslist
import scala.collection.immutable.Nil

/**
  * A class for immutable linked lists of at least two elements
  *
  */
final case class TwoPlusList[+A] private (head: A, body: A, tail: List[A]) extends IterableOnce[A] {

  import TwoPlusList._

  /**
    * Return the head, body, and tail as a single list
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.toList
    * val res0: List[Int] = List(1, 2, 3, 4, 5)
    * }}}
    * @return list with all elements of this `TwoPlusList`
    */
  def toList: List[A] = head :: body :: tail

  /**
    * Selects the last element of the list
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.last
    * val res0: Int = 5
    * }}}
    * @return last element of this list
    */
  def last: A = if(tail.isEmpty) body else tail.last

  /**
    * Selects all elements except the last
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.init
    * val res0: List[Int] = List(1, 2, 3, 4)
    * }}}
    * @return list with the last element removed
    */
  def init: List[A] = if(tail.isEmpty) List(head) else head :: body :: tail.init

  /**
    * The size of this TwoPlusList
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.size
    * val res0: Int = 5
    * }}}
    * @return size of this list. Will always be >= 2
    */
  def size: Int = 2 + tail.size

  /**
    * Alias for `size`
    *
    * @return length of this list
    */
  def length: Int = size

  /**
    * Applies f to all elements of this structure
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.map(_ + 1)
    * val res0: com.brianpritchett.twopluslist.TwoPlusList[Int] = TwoPlusList(2,3,List(4, 5, 6))
    * }}}
    * @param f
    * @return new `TwoPlusList` transformed by f
    */
  def map[B](f: A => B): TwoPlusList[B] = TwoPlusList(f(head), f(body), tail.map(f))

  def ++[AA >: A](l: List[AA]): TwoPlusList[AA] = concat(l)

  def concat[AA >: A](other: List[AA]): TwoPlusList[AA] = TwoPlusList(head, body, tail ::: other)
  def concatTPL[AA >: A](other: TwoPlusList[AA]): TwoPlusList[AA] = TwoPlusList(head, body, tail ::: other.toList)

  def flatMap[B](f: A => TwoPlusList[B]): TwoPlusList[B] = f(head) ++ f(body).toList ++ tail.flatMap(f andThen (l => l.toList))

  def prepend[AA >: A](a: AA): TwoPlusList[AA] = TwoPlusList(a, head, body :: tail)

  def ::[AA >: A](a: AA): TwoPlusList[AA] = prepend(a)

  def :+[AA >: A](a: AA): TwoPlusList[AA] = append(a)

  def append[AA >: A](a: AA): TwoPlusList[AA] = TwoPlusList(head, body, tail :+ a)

  def :::[AA >: A](other: TwoPlusList[AA]): TwoPlusList[AA] = other.concatTPL(this)

  /**
    * Keep elements matching the predicate
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
  def filter(p: A => Boolean): List[A] = {
    val includeHead = p(head)
    val includeBody = p(body)
    val fTail = tail.filter(p)

    if(includeHead && includeBody) head :: body :: fTail
    else if (includeHead) head :: fTail
    else if (includeBody) body :: fTail
    else fTail
  }

  /**
    * Inverse of `filter`
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
  def filterNot(p: A => Boolean): List[A] = filter(p andThen (!_))

  /**
    * Builds a new `List` by applying a partial function to
    * all the elements from this `TwoPlusList` on which the function is defined
    * {{{
    * scala> import com.brianpritchett.twopluslist.TwoPlusList
    * scala> val tpl = TwoPlusList.of(1, 2, 3, 4, 5)
    * scala> tpl.collect { case v if v > 2 => v }
    * val res0: List[Int] = List(3, 4, 5)
    * }}}
    * @param pf 
    */
  def collect[B](pf: PartialFunction[A, B]): List[B] = {
    val headDefined = pf.isDefinedAt(head)
    val bodyDefined = pf.isDefinedAt(body)
    val tailC = tail.collect(pf)

    if(headDefined && bodyDefined) pf.apply(head) :: pf.apply(body) :: tailC
    else if (headDefined) pf.apply(head) :: tailC
    else if (bodyDefined) pf.apply(body) :: tailC
    else tailC
  }
  /**
    * find the first element matching the predicate, if one exists
    *
    */
  def find(p: A => Boolean): Option[A] = {
    if(p(head)) Some(head)
    else if(p(body)) Some(body)
    else tail.find(p)
  }

  /**
    * Check where all elements match the predicate
    *
    */
  def forall(p: A => Boolean): Boolean = p(head) && p(body) && tail.forall(p)

  /**
    * Check if this `TwoPlusList` contains the element
    *
    */
  def contains[A1 >: A](a: A1): Boolean = (head == a) || (body == a) || tail.contains(a)

  def apply(i: Int) = {
    if (i < 0) throw new IndexOutOfBoundsException(i)
    if (i == 0) head else if (i == 1) body else tail(i - 2)
  }

  def flatten[B](implicit a: A => scala.collection.IterableOnce[B]): List[B] =
    toList.flatten

  override def iterator: Iterator[A] = toList.iterator

  /**
    * Build a new list by removing the first n elements of this list
    *
    */
  def drop(n: Int): List[A] = {
    if(n <= 0) toList
    else if (n == 1) body :: tail
    else if (n == 2) tail
    else tail.drop(n - 2)
  }

  /**
    * Left associative fold on the list useing op.
    *
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val rHead = op(z, head)
    val rBody = op(rHead, body)
    tail.foldLeft(rBody)(op)
  }

  /**
    * Build a new list out of the first n elements of this list
    *
    */
  def take(n: Int): List[A] = {
    if (n < 1) List.empty
    else if (n == 1) List(head)
    else if (n == 2) List(head, body)
    else head :: body :: tail.take(n - 2)
  }

  val isEmpty = false
}

object TwoPlusList {

  /**
    * Create a `TwoPlusList` from two or more elements
    *
    */
  def of[A](a1: A, a2: A, a3: A*): TwoPlusList[A] = new TwoPlusList(a1, a2, a3.toList)

  /**
    * Create a `TwoPlusList` from two elements
    *
    */
  def two[A](a1: A, a2: A): TwoPlusList[A] = of(a1, a2)

   /**
    * Create a `TwoPlusList` from a `List`.
    *
    * The result will be `None` if the input list size is less than 2
    *  and `Some` wrapping a`TwoPlusList` otherwise.
    *
    * @see [[fromListUnsafe]] for an unsafe version that throws an exception if
    * the input list size is less than 2.
    */
  def fromList[A](ls: List[A]): Option[TwoPlusList[A]] = ls match {
    case head :: body :: tail  => Some(TwoPlusList(head, body, tail))
    case _ => None
  }

  /**
    * Create a `TwoPlusList` from a `List`, or throw an
    * `IllegalArgumentException` if the input list is empty.
    *
    * @see [[fromList]] for a safe version that returns `None` if the input list
    * is empty.
    */
  def fromListUnsafe[A](ls: List[A]) = fromList(ls).getOrElse(throw new IllegalArgumentException(s"Cannot create a TwoPlusList from a list of size ${ls.size}"))
}

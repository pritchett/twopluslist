package com.brianpritchett.twopluslist
import scala.collection.immutable.Nil

final case class TwoPlusList[+A] private (head: A, body: A, tail: List[A]) {
  import TwoPlusList._

  def toList: List[A] = head :: body :: tail
  def last: A = if(tail.isEmpty) body else tail.last
  def init: List[A] = if(tail.isEmpty) List(head) else head :: body :: tail.init
  def size: Int = 2 + tail.size
  def length: Int = size

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

  def filter(p: A => Boolean): List[A] = {
    val includeHead = p(head)
    val includeBody = p(body)
    val fTail = tail.filter(p)

    if(includeHead && includeBody) head :: body :: fTail
    else if (includeHead) head :: fTail
    else if (includeBody) body :: fTail
    else fTail
  }

  def filterNot(p: A => Boolean): List[A] = filter(p andThen (b => !b))

  def collect[B](pf: PartialFunction[A, B]): List[B] = {
    val headDefined = pf.isDefinedAt(head)
    val bodyDefined = pf.isDefinedAt(body)
    val tailC = tail.collect(pf)

    if(headDefined && bodyDefined) pf.apply(head) :: pf.apply(body) :: tailC
    else if (headDefined) pf.apply(head) :: tailC
    else if (bodyDefined) pf.apply(body) :: tailC
    else tailC
  }

  def find(p: A => Boolean): Option[A] = {
    if(p(head)) Some(head)
    else if(p(body)) Some(body)
    else tail.find(p)
  }

}

object TwoPlusList {

  def of[A](a1: A, a2: A, as: A*): TwoPlusList[A] = new TwoPlusList(a1, a2, as.toList)

  def two[A](a1: A, a2: A): TwoPlusList[A] = of(a1, a2)

  def fromList[A](ls: List[A]): Option[TwoPlusList[A]] = ls match {
    case head :: body :: tail  => Some(TwoPlusList(head, body, tail))
    case _ =>  None
  }
}

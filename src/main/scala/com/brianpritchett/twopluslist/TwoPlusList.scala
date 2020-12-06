package com.brianpritchett.twopluslist
import scala.collection.immutable.Nil

class TwoPlusList[+A] private (head: A, body: A, tail: List[A]) {

  def toList: List[A] = head :: body :: tail
}

object TwoPlusList {
  def apply[A](a1: A, a2: A, as: A*): TwoPlusList[A] = new TwoPlusList(a1, a2, as.toList)

  def two[A](a1: A, a2: A): TwoPlusList[A] = apply(a1, a2)

  def fromList[A](ls: List[A]): Option[TwoPlusList[A]] = ls match {
    case head :: body :: Nil  => Some(two(head, body))
    case head :: body :: rest => Some(apply(head, body, rest: _*))
    case _ =>  None
  }
}

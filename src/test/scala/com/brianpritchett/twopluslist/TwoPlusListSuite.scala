package com.brianpritchett.twopluslist
import munit._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Gen

class TwoPlusListSuite extends ScalaCheckSuite {

  val atLeastTwo = Gen.listOf(Gen.double).suchThat(_.size >= 2)
  val zeroOrOne = Gen.choose(0, 1).map(n => List.fill(n)(n))
  val two = Gen.listOfN(2, Gen.double)

  property("fromList succeedes when list size > 1") {
    forAll(atLeastTwo) { ns: List[Double] =>
      TwoPlusList.fromList(ns).isDefined
    }
  }

  property("fromList returns None when list size <= 1") {
    forAll(zeroOrOne) { ns => 
        TwoPlusList.fromList(ns).isEmpty
    }
  }

  property("size returns number of elements in list") {
    forAll { (n1: Int, n2: Int, n3: Option[List[Int]]) =>
      n3 match {
	case Some(v) if v.nonEmpty => TwoPlusList.of(n1, n2, v: _*).size == 2 + v.size
	case _ => TwoPlusList.two(n1, n2).size == 2
      }
    }
  }

  property("size and length are the same") {
    forAll { (n1: Int, n2: Int, n3: Option[List[Int]]) =>
      val l = n3 match {
	case Some(v) => TwoPlusList.of(n1, n2, v: _*)
	case None => TwoPlusList.two(n1, n2)
      }

      l.length == l.size
    }

  }

  property("last for list of two always returns second element") {
    forAll { (n1: Int, n2: Int, n3: Option[List[Int]]) =>
      n3 match {
	case Some(ns) if ns.nonEmpty => TwoPlusList.of(n1, n2, ns: _*).last == ns.last
	case _ => TwoPlusList.two(n1, n2).last == n2
      }
    }
  }
}


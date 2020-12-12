package com.brianpritchett.twopluslist
import munit._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary

class TwoPlusListSuite extends ScalaCheckSuite {

  val atLeastTwo = Gen.listOf(Gen.double).suchThat(_.size >= 2)
  val zeroOrOne = Gen.choose(0, 1).map(n => List.fill(n)(n))
  val two = Gen.listOfN(2, Gen.double)

  implicit def tplGen[A](implicit a: Arbitrary[A]) =
    for {
      n1 <- arbitrary[A]
      n2 <- arbitrary[A]
      n3 <- Gen.option(Gen.listOf(arbitrary[A]))
    } yield withOptional(n1, n2, n3)

  implicit def tplArb[A](implicit a: Arbitrary[A]) = Arbitrary(tplGen[A])

  def withOptional[A](n1: A, n2: A, n3: Option[List[A]]) =
    n3 match {
      case Some(v) if v.nonEmpty => TwoPlusList.of(n1, n2, v: _*)
      case _                     => TwoPlusList.two(n1, n2)
    }

  property("fromList succeedes when list size > 1") {
    forAll { ns: List[Int] =>
      (ns.size > 1) ==>
        TwoPlusList.fromList(ns).isDefined
    }
  }

  property("fromList returns None when list size <= 1") {
    forAll(zeroOrOne) { ns =>
      TwoPlusList.fromList(ns).isEmpty
    }
  }

  property("size returns number of elements in list") {
    forAll(Gen.posNum[Int]) { n: Int =>
      (n > 1 && n < 100000) ==>
        TwoPlusList.fromList(List.fill(n)(n)).exists(_.size == n)
    }
  }

  property("size and length are the same") {
    forAll { l: TwoPlusList[Int] =>
      l.length == l.size
    }
  }

  property("last gets the element at the size - 1 of the list") {
    forAll { l: TwoPlusList[Int] =>
      l.last == l(l.size - 1)
    }
  }

  property("size of concatenated list is sum of sizes of contituent lists") {
    forAll { (l: TwoPlusList[Int], r: TwoPlusList[Int]) =>
      (l ::: r).size == l.size + r.size
    }
  }

  property("concatenated list has all elements of both lists") {
    forAll { (l: TwoPlusList[Int], r: TwoPlusList[Int]) =>
      val cat = l ::: r
      cat.forall(e => l.contains(e) || r.contains(e)) &&
      l.forall(cat.contains) &&
      r.forall(cat.contains)
    }
  }

  property("toList constructs a correct list") {
    forAll { (n1: Int, n2: Int, ns: List[Int]) =>
      TwoPlusList.of(n1, n2, ns: _*).toList == n1 :: n2 :: ns
    }
  }

  property("init gives list with one less element") {
    forAll { tpl: TwoPlusList[Int] =>
      tpl.init.size == tpl.size - 1
    }
  }

  property("init is same as toList then init") {
    forAll { tpl: TwoPlusList[Int] =>
      tpl.init == tpl.toList.init
    }
  }

  property(
    "concat two lists then turn to TPL is same as turning both to TPL then concat"
  ) {
    forAll { (l: List[Int], r: List[Int]) =>
      (l.size > 1 && r.size > 1) ==> {
        val lOpt = TwoPlusList.fromList(l)
        val rOpt = TwoPlusList.fromList(r)

        val t = lOpt.flatMap(ltpl => rOpt.map(rtpl => ltpl.concatTPL(rtpl)))

        TwoPlusList.fromList(l ++ r) == t
      }
    }
  }

  property("map then toList is same as toList then map") {
    forAll(
      Gen.posNum[Int],
      arbitrary[Function1[Int, Int]]
    ) { (n: Int, f: Int => Int) =>
      (n > 1 && n < 100000) ==> {
        val l = List.fill(n)(n)
        val tpl = TwoPlusList.fromList(l)
        tpl.map(_.map(f).toList) == tpl.map(_.toList.map(f))
      }
    }
  }
}

package fpinscala.monoids

import org.specs2.mutable.Specification

class ListSpec extends Specification {

  val listA = List(1, 3)
  val listB = List(2, 4)
  val listC = List(5, 7)

  "for comprehension" should {
    "generate all possible results" in {
      val result = for {
        a <- listA
        b <- listB
        c <- listC
      } yield (a + b + c)

      result must_== List(8, 10, 10, 12, 10, 12, 12, 14)
    }
  }

  "flatMap and Map" should {
    "give same results as for comprehension" in {
      val result = listA.flatMap(a => listB.flatMap(b => listC.map(c => a + b + c)))

      result must_== List(8, 10, 10, 12, 10, 12, 12, 14)
    }
  }

}


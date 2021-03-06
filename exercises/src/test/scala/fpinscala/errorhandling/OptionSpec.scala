package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "map" should {
    "transform None to None" in {
      None.map(a => 1) must_== None
    }

    "transform Some" in {
      Some(1).map(_ => "abc") must_== Some("abc")
    }
  }

  "map2" should {
    "transfomr None and None to None" in {
      Option.map2(None, None)((a, b) => "123") must_== None
    }

    "transfomr Some and Some to Some" in {
      Option.map2(Some(1), Some(2))((a, b) => 1 + 2) must_== Some(1 + 2)
    }

    "transform None and Some to None" in {
      Option.map2(None, Some(2))((a, b) => 1 + 2) must_== None
    }

    "transform Some and None to None" in {
      Option.map2(Some(1), None)((a, b) => 1 + 2) must_== None
    }
  }

  "sequence" should {
    "be None when the list contains a None" in {
      Option.sequence(List[Option[Nothing]](None)) must_== None
      Option.sequence(List(Some(1), None)) must_== None
      Option.sequence(List(Some(1), None, Some(2))) must_== None
      Option.sequence(List(None, Some(1))) must_== None
    }

    "Some list" in {
      Option.sequence(List(Some(1))) must_== Some(List(1))
      Option.sequence(List(Some(1), Some(2))) must_== Some(List(1, 2))
    }
  }

  "sequence_1" should {
    "be None when the list contains a None" in {
      Option.sequence_1(List[Option[Nothing]](None)) must_== None
      Option.sequence_1(List(Some(1), None)) must_== None
      Option.sequence_1(List(None, Some(1))) must_== None
    }

    "Some list" in {
      Option.sequence_1(List(Some(1))) must_== Some(List(1))
      Option.sequence_1(List(Some(1), Some(2))) must_== Some(List(1, 2))
    }
  }

  "sequenceViaTraverse" should {
    "be None when the list contains a None" in {
      Option.sequenceViaTraverse(List[Option[Nothing]](None)) must_== None
      Option.sequenceViaTraverse(List(Some(1), None)) must_== None
      Option.sequenceViaTraverse(List(None, Some(1))) must_== None
    }

    "Some list" in {
      Option.sequenceViaTraverse(List(Some(1))) must_== Some(List(1))
      Option.sequenceViaTraverse(List(Some(1), Some(2))) must_== Some(List(1, 2))
    }

  }

  "traverse" should {
    "give Some list" in {
      Option.traverse(List(1, 2))(a => Some(a + 1)) must_== Some(List(2, 3))
    }
    "give None if the function maps to a None" in {
      Option.traverse(List(1, 2))(a => None) must_== None
      Option.traverse(List(Some(1), None))(a => a) must_== None
      Option.traverse(List(None, Some(1)))(a => a) must_== None
    }
  }

  "traverseViaSequence" should {
    "give Some list" in {
      Option.traverseViaSequence(List(1, 2))(a => Some(a + 1)) must_== Some(List(2, 3))
    }
    "give None if the function maps to a None" in {
      Option.traverseViaSequence(List(1, 2))(a => None) must_== None
      Option.traverseViaSequence(List(Some(1), None))(a => a) must_== None
      Option.traverseViaSequence(List(None, Some(1)))(a => a) must_== None
    }
  }
}

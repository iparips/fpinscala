package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "toList" >> {

    "given empty stream it should produce an empty list" >> {
      Stream().toList shouldEqual List()
    }

    "given non empty stream it should evaluate to a list" >> {
      Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
    }

  }

  "take" >> {

    "given a stream with more than 2 elements, take 2 returns the first 2" >> {
      Stream(1, 2, 3, 4).take(2) shouldEqual Stream(1, 2)
    }

    "given an empty stream, take 2 returns empty stream" >> {
      Stream().take(2) shouldEqual Stream()
    }

    "given a stream with 1 element, take 2 returns that element" >> {
      Stream(1).take(2) shouldEqual Stream(1)
    }

  }

  "drop" >> {

    "given a stream with more than 2 elements, it returns the rest" >> {
      Stream(1, 2, 3, 4).drop(2) shouldEqual Stream(3, 4)
    }

    "given a stream with 2 elements, it returns empty stream" >> {
      Stream(1, 2).drop(2) shouldEqual Empty
    }

    "given a stream with less than 2 elements, it returns empty stream" >> {
      Stream(1).drop(2) shouldEqual Empty
    }

  }

  "takeWhile" >> {

    "given the predicate rejects the 2nd element, return the first one" >> {
      Stream(1, 2, 3).takeWhile(_ != 2) shouldEqual Stream(1)
    }

    "given the predicate rejects the 1st element, return Empty" >> {
      Stream(1, 2, 3).takeWhile(_ != 1) shouldEqual Empty
    }

    "given no elements match predicate, return Empty" >> {
      Stream(1, 2, 3).takeWhile(_ == 4) shouldEqual Empty
    }

  }

  "forAll" >> {

    "given there are elements matching predicate, return true" >> {
      Stream(1, 2, 3, 4).forAll(_ > 0) shouldEqual true
    }

    "given no elements match, return false" >> {
      Stream(1, 2, 3, -4).forAll(_ > 0) shouldEqual false
    }

  }

  "headOption" >> {

    "given empty Stream, returns None" >> {
      Empty.headOption shouldEqual None
    }

    "given non-empty Stream, returns Some(head)" >> {
      Stream(1, 2, 3).headOption shouldEqual Some(1)
    }

  }

}

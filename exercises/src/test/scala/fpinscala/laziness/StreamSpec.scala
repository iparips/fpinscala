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

}

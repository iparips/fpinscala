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

}

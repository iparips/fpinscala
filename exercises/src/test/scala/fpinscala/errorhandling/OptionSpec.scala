package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  val f = (a: Int) => a + 10 // f can be anything
  // Q: What if f itself throws an exception,
  // is it a responsibility of map to catch it and return None?

  "map" >> {

    "given Some map transforms" >> {
      Some(1).map(f) shouldEqual Some(11)
    }

    "given None map returns None" >> {
      None.map(f) shouldEqual None
    }

  }

  "flatMap" >> {

    val f: Int => Option[Int] = Some(_)

    "given Some it unwraps the value" >> {
      Some(1).flatMap(f) shouldEqual Some(1)
    }

    "given None it returns default value" >> {
      None.flatMap(f) shouldEqual None
    }

  }

  "getOrElse" >> {

    "given Some it unwraps the value" >> {
      Some(1).getOrElse(2) shouldEqual 1
    }

    "given None it returns default value" >> {
      None.getOrElse(2) shouldEqual 2
    }

  }

}

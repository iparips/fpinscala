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

  "map2" >> {

    // what function might I want to give to map 2
    val f = (a: String, b: String) => a + b

    "given two Some applies function to unwrapped values" >> {
      Option.map2(Some("Hello"), Some("World"))(f) shouldEqual Some("HelloWorld")
    }

    "given one None it returns None" >> {
      Option.map2(Some("Hello"), None)(f) shouldEqual None
    }

  }

  // every operation in the list should succeed,
  // otherwise the aggregate operation should fail
  "sequence" >> {

    "converts a list of None to None" >> {
      Option.sequence(List[Option[Int]](None)) shouldEqual None
    }

    "converts empty list of options to an empty option of list" >> {
      Option.sequence(List[Option[Int]]()) shouldEqual Some(List())
    }

    "returns None when any list members are None" >> {
      Option.sequence(List[Option[Int]](None, Some(1))) shouldEqual None
    }

    "converts list of options to an option of list" >> {
      Option.sequence(List[Option[Int]](Some(1))) shouldEqual Some(List(1))
    }

  }

  // aggregates non-None elements of a list
    "flatten" >> {

      "empty list stays an empty list" >> {
        Option.flatten(List[Option[Int]]()) shouldEqual List()
      }

      "removes None members" >> {
        Option.flatten(List[Option[Int]](None)) shouldEqual List()
      }

      "retrieves values from options" >> {
        Option.flatten(List[Option[Int]](Some(1))) shouldEqual List(1)
      }

      "strips Nones & extracts values from Options" >> {
        Option.flatten(List[Option[Int]](Some(1), None)) shouldEqual List(1)
      }

    }

  "filter" >> {

    "returns None when option is None" >> {
      None.filter((x) => x == 1) shouldEqual None
    }

    "returns None when the function yields false" >> {
      Some(1).filter((x) => x == 2) shouldEqual None
    }

    "returns itself when the function yields true" >> {
      Some(1).filter((x) => x == 1) shouldEqual Some(1)
    }

  }

  "traverse" >> {

    "given empty list returns Option of empty list" >> {
      Option.traverse(List())(x => Some(x)) shouldEqual Some(List())
    }

    "given a non-empty list, return option of that list" >> {
      Option.traverse(List(1))(x => Some(x)) shouldEqual Some(List(1))
    }

  }

}

package prv.zielony.scala.tutorials

import org.junit.runner.RunWith
import org.scalacheck.{Prop, Gen}
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.{Checkers, PropertyChecks}

/**
 * Created by zielony on 02.02.16.
 */
@RunWith(classOf[JUnitRunner])
class PeselValidatorTest extends FunSuite with PropertyChecks with Checkers {

  val digitGenerator:Gen[Int] = Gen.oneOf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  val yearGenerator:Gen[(Int, Int)] = (for{
    digit1 <- Gen.oneOf( 1, 2, 3, 4, 5, 6, 7, 8, 9)
    digit2 <- digitGenerator
  } yield((digit1, digit2)))

  val monthGenerator:Gen[(Int, Int)] = Gen.oneOf((0, 1),(0, 2),(0, 3),(0 ,4),(0, 5), (0, 6), (0, 7),
    (0,8), (0,9), (1,0), (1,1), (1,2))

  val dayGenerator:Gen[(Int, Int)] = (for {

    digit1 <- Gen.choose(0, 3)
    digit2 <- digitGenerator

  } yield ((digit1, digit2))).suchThat({input =>
    if(input._1 == 3) {
      input._2 == 0
    }
    else if(input._1 == 0) {
      input._2 != 0
    }
    else {
      true
    }
  })

  val factors = List(1, 3, 1, 9, 7, 3, 1, 9, 7, 3, 1);

  val correctPeselListGenerator:Gen[List[Int]] = (for{
      year <- yearGenerator
      month <- monthGenerator
      day <- dayGenerator
      digit7 <- digitGenerator
      digit8 <- digitGenerator
      digit9 <- digitGenerator
      digit10 <- digitGenerator
      digit11 <- digitGenerator
  } yield(List(year._1, year._2, month._1, month._2, day._1, day._2,
      digit7, digit8, digit9, digit10, digit11))).suchThat(list => {

    val result = list.zipWithIndex.map(tuple => {
      tuple._1 * factors(factors.size - 1 -tuple._2)
    }).fold(0) { (element1, element2) =>
      element1 + element2
    }

    ((result % 10) == 0)
  })

  val correctPeselGenerator:Gen[Long] = (
      for {
        list <- correctPeselListGenerator
      } yield(list.zipWithIndex.map { tuple =>
            tuple._1*Math.pow(10, (list.size - 1 - tuple._2)).asInstanceOf[Long]
      }.fold(0L) { (element1, element2) =>
        element1 + element2
      })
    )

  val incorrectPeselGenerator = Gen.choose((Math.pow(10,10)+1).asInstanceOf[Long], (Math.pow(10, 11)-1).asInstanceOf[Long])
    .suchThat( input => {
    val factors = List(1, 3, 1, 9, 7, 3, 1, 9, 7, 3, 1);
    var sum:Long = 0;
    for( i <- (0 until factors.size)) {
      sum += factors(i)*(Math.floorDiv(input, Math.pow(10, i).asInstanceOf[Long])%10)
    }

    (sum%10 != 0)

  })

  val tooShortPeselGenerator = Gen.choose(0L, Math.pow(10,10).asInstanceOf[Long])

  val tooLongPeselGenerator = Gen.choose(Math.pow(10, 11).asInstanceOf[Long], Math.pow(10, 12).asInstanceOf[Long])

  test("Should accept correct PESEL") {
    check(Prop.forAll(correctPeselGenerator) { input =>
      PeselValidator.validate(input)
    })
  }

  test("Should not accept incorrect PESEL") {
    check(Prop.forAll(incorrectPeselGenerator) { input =>
      !PeselValidator.validate(input)
    })
  }

  test("Should not accept too short PESEL") {
    check(Prop.forAll(tooShortPeselGenerator) { input =>
      !PeselValidator.validate(input)
    })
  }

  test("Should not accept too long PESEL") {
    check(Prop.forAll(tooLongPeselGenerator) { input =>
      !PeselValidator.validate(input)
    })
  }
}

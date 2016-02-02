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

  val correctPeselGenerator = Gen.choose((Math.pow(10,10)+1).asInstanceOf[Long], (Math.pow(10, 11)-1).asInstanceOf[Long])
    .suchThat( input => {
      val factors = List(1, 3, 1, 9, 7, 3, 1, 9, 7, 3, 1);
      var sum:Long = 0;
      for( i <- (0 until factors.size)) {
        sum += factors(i)*(Math.floorDiv(input, Math.pow(10, i).asInstanceOf[Long])%10)
      }

      (sum%10 == 0)

    })

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

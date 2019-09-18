package prv.zielony.scala.tutorials

import org.junit.runner.RunWith
import org.scalacheck.{Prop, Gen}
import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.{PropertyChecks, Checkers}

/**
 * Created by zielony on 01.02.16.
 */
@RunWith(classOf[JUnitRunner])
class FactorialTest extends FunSuite with PropertyChecks with Checkers{

  val positiveIntGenerator = Gen.choose(1, 1000);

  val nonNegativeIntGenerator = Gen.choose(0, 1000);

  val negativeIntGenerator = Gen.choose(-1000, -1);

  val zeroGenerator = Gen.const(0)

  test("Defined for nonnegative integers") {
    check(Prop.forAll(nonNegativeIntGenerator) { input =>
      Algorithmics.factorial(input).isDefined
    })
  }

  test("Undefined for negative integers") {
    check(Prop.forAll(negativeIntGenerator) { input =>
      Algorithmics.factorial(input).isEmpty
    })
  }

  test("Equal to 1 for 0") {
    check(Prop.forAll(zeroGenerator) { input =>
      val result = Algorithmics.factorial(input);
      result.isDefined && result.get == 1
    })
  }

  test("Recursive") {
   check(Prop.forAll(positiveIntGenerator) { input =>
     val result = Algorithmics.factorial(input)

     result.isDefined && (result.get == input*Algorithmics.factorial(input-1).get);
   })
  }

}

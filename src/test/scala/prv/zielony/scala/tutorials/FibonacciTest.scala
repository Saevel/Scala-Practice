package prv.zielony.scala.tutorials

import org.junit.runner.RunWith
import org.scalacheck.{Prop, Gen}
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.{Checkers, PropertyChecks}

/**
 * Created by zielony on 01.02.16.
 */
@RunWith(classOf[JUnitRunner])
class FibonacciTest extends FunSuite with PropertyChecks with Checkers {

  val integerGreaterThanOneGenerator = Gen.choose(2, 10);

  val zeroGenerator = Gen.const(0)

  val oneGenerator = Gen.const(1);

  val nonNegativeIntGenerator = Gen.choose(0, 10);

  val negativeIntGenerator = Gen.choose(-100, -1);

  test("Defined for non-negative integers") {
    check(Prop.forAll(nonNegativeIntGenerator) { input =>
      Algorithmics.fibonacci(input).isDefined
    })
  }

  test("Undefined for negative integers") {
    check(Prop.forAll(negativeIntGenerator) {input =>
      Algorithmics.fibonacci(input).isEmpty
    })
  }

  test("Equal to 1 for 0") {
    check(Prop.forAll(zeroGenerator) { input =>
      val result = Algorithmics.fibonacci(input)

      result.isDefined && (result.get == 1)
    })
  }

  test("Equal to 2 for 1") {
    check(Prop.forAll(oneGenerator) { input =>
      val result = Algorithmics.fibonacci(input)

      result.isDefined && (result.get == 2)
    })
  }

  test("Recursive") {
    check(Prop.forAll(integerGreaterThanOneGenerator) { input =>
      val result = Algorithmics.fibonacci(input);

      result.isDefined && (result.get == (Algorithmics.fibonacci(input-1).get + Algorithmics.fibonacci(input-2).get))
    })
  }
}

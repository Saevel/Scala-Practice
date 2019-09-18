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

  val digitGenerator:Gen[Int] = Gen.choose(1, 9)

  val yearGenerator:Gen[(Int, Int)] = Gen.choose(0, 99).map(i => (digit(i, 1), digit(i, 2)))

  val monthGenerator:Gen[(Int, Int)] = Gen.choose(1, 12).map(i => (digit(i, 1), digit(i, 2)))

  val dayGenerator:Gen[(Int, Int)] = Gen.choose(1, 28).map(i => (digit(i, 1), digit(i, 2)))

  val tenFactors = List(1, 3, 7, 9, 1, 3, 7, 9, 1, 3)

  val peselBaseGenerator: Gen[List[Int]] = for {
    year <- yearGenerator
    month <- monthGenerator
    day <- dayGenerator
    digit7 <- digitGenerator
    digit8 <- digitGenerator
    digit9 <- digitGenerator
    digit10 <- digitGenerator
  } yield(List(year._1, year._2, month._1, month._2, day._1, day._2,
    digit7, digit8, digit9, digit10))

  val correctPeselGenerator: Gen[List[Int]] = peselBaseGenerator.map(peselBase =>
    peselBase ++ List((10 - digit(linearCombination(peselBase, tenFactors), 1)) % 10)
  )

  val incorrectPeselGenerator: Gen[List[Int]] = peselBaseGenerator.map(peselBase =>
    peselBase ++ List(9 - digit(linearCombination(peselBase, tenFactors), 1))
  )

  val tooShortPeselGenerator = Gen.choose(0L, Math.pow(10,10).asInstanceOf[Long])

  val tooLongPeselGenerator = Gen.choose(Math.pow(10, 11).asInstanceOf[Long], Math.pow(10, 12).asInstanceOf[Long])

  test("Should accept correct PESEL") {
    check(Prop.forAllNoShrink(correctPeselGenerator) { input =>
      PeselValidator.validate(input)
    })
  }

  test("Should not accept incorrect PESEL") {
    check(Prop.forAllNoShrink(incorrectPeselGenerator) { input =>
      !PeselValidator.validate(input)
    })
  }

  def linearCombination(a: List[Int], b: List[Int]): Int = a.zip(b).foldLeft(0)((accumulator, tuple) => tuple match {
    case (x, y) => accumulator + x * y
  })

  implicit class LongFromIntList(digits: List[Int]){
    def asLong: Long = digits.zipWithIndex.map{ case (digit, i) =>
      digit * Math.pow(10, (digits.length - i - 1)).toLong
    }.foldLeft(0L)((accumulator, x) => accumulator + x)
  }

  def digit(i: Long, n: Int): Int = if( i > (10 ^ (n-1)))
    (( i / Math.pow(10, (n - 1))).toLong % 10).toInt
  else
    0

  def digit(i: Int, n: Int): Int = if( i > (10 ^ (n-1)))
    (( i / Math.pow(10, (n - 1))).toLong % 10).toInt
  else
    0
}

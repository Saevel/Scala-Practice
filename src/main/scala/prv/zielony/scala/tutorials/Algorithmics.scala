package prv.zielony.scala.tutorials

/**
 * Created by zielony on 01.02.16.
 */
object Algorithmics {

  /**
    * The goal of the first exercise is to implement the factorial function defined as:
    *    - for negative numbers (None)
    *    - for zero: 1
    *    - for positive numbers: factorial(n) = n * factorial(n-1)
    */

  def factorial(input:Int):Option[Int] = {

    if(input == 0) {
      return Some(1)
    }
    else if(input > 0) {
      return Some(input * factorial(input-1).get)
    }
    else {
      return None;
    }
  }

  /**
   * The goal is to implement a function calculating Fibonacci numbers, defined as:
   *  - for negative numbers: undefined (None)
   *  - for zero: 1
   *  - for one: 2
   *  - for positive numbers: f(n) = f(n-1) + f(n-2)
   */

  def fibonacci(n:Int):Option[Int] = {

    if(n == 0) {
      return Some(1);
    }
    else if(n == 1) {
      return Some(2);
    }
    else if(n > 1) {
      return Some(fibonacci(n-1).get + fibonacci(n-2).get)
    }
    else {
      return None;
    }
  }
}

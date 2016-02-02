package prv.zielony.scala.tutorials

/**
 * Created by zielony on 01.02.16.
 */
object Algorithmics {

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

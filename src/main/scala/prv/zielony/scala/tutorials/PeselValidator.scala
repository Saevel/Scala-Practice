package prv.zielony.scala.tutorials

/**
 * Created by zielony on 02.02.16.
 */
object PeselValidator {

  /**
    * The aim is to validate the PESEL number, which is more or less a Polish ID number.
    * The validation should go as follows:
    * 1. The number has to have 11 digits
    * 2. The following algorithm should hold:
    *
    *   We multiply multiply digits respectively:
    *   - first by 1
    *   - second by 3
    *   - third by 7
    *   - fourth by 9
    *   - fifth by 1
    *   - sixth by 3
    *   - seventh by 7
    *   - eight by 9
    *   - ninth by 1
    *   - tenth by 3
    *   - eleventh by 1
    *   where the first digit is the one on the left-hand side as the number is written, i.e. for
    *   1234 the first digit is 1 and the fourth is 4.
    *   Next, we sum all the results. If the final result is divisible by 10, the PESEL number is correct.
    *   Otherwise, it isn't.
    */

  def validate(pesel:Long):Boolean = {

    if(pesel >= (Math.pow(10, 10)) && pesel <= (Math.pow(10,11)) ) {
      val factors = List(1, 3, 1, 9, 7, 3, 1, 9, 7, 3, 1);

      var sum:Long = 0;
      for( i <- (0 until factors.size)) {
        sum += factors(i)*(Math.floorDiv(pesel, Math.pow(10, i).asInstanceOf[Long])%10)
      }

      return (sum%10 == 0)
    }
    else {
      return false;
    }
  }
}

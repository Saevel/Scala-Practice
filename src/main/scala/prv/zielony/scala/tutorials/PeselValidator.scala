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
    *   - third by 1
    *   - fourth by 9
    *   - fifth by 7
    *   - sixth by 3
    *   - seventh by 1
    *   - eight by 9
    *   - ninth by 7
    *   - tenth by 3
    *   - eleventh by 1
    *   where the digits are numerated from the highest to the lowest.
    *   Next, we sum all the results. If the final result is divisible by 10, the PESEL number is correct.
    *   Otherwise, it isn't.
    */

  def validate(pesel:Long):Boolean = ???
}
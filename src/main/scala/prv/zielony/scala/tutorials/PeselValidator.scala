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

  def validate(pesel:List[Int]):Boolean={

    if (pesel.length == 11){

      var controlSum = 0
      for (i <- 0 to pesel.length-1) {
        i match {
          case 0 => {
            controlSum = controlSum + pesel(i) * 1
          }
          case 1 => {
            controlSum = controlSum + pesel(i)  * 3
          }
          case 2 => {
            controlSum = controlSum + pesel(i)  * 7
          }
          case 3 => {
            controlSum = controlSum + pesel(i)  * 9
          }
          case 4 => {
            controlSum = controlSum + pesel(i) * 1
          }
          case 5 => {
            controlSum = controlSum + pesel(i)  * 3
          }
          case 6 => {
            controlSum = controlSum + pesel(i)  * 7
          }
          case 7 => {
            controlSum = controlSum + pesel(i) * 9
          }
          case 8 => {
            controlSum = controlSum + pesel(i)  * 1
          }
          case 9 => {
            controlSum = controlSum + pesel(i)  * 3
          }
          case 10 => {
            controlSum = controlSum + pesel(i)  * 1
          }

        }

      }

      if (controlSum % 10 == 0) {
        println ("Prawidlowy numer PESEL")
        return true
      } else {
        println ("Nieprawidlowy numer PESEL")
        return false
      }
    }else{return false}}
}


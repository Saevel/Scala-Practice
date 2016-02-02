package prv.zielony.scala.tutorials

/**
 * Created by zielony on 02.02.16.
 */
object PeselValidator {
/*
  Pierwszą cyfrę mnożymy przez 1, drugą cyfrę mnożymy przez 3,
   trzecią cyfrę mnożymy przez 7, czwarta cyfrę mnożymy przez 9,
    piątą cyfrę mnożymy przez 1, szóstą cyfrę mnożymy przez 3,
     siódmą cyfrę mnożymy przez 7, ósmą cyfrę mnożymy przez 9,
      dziewiątą cyfrę mnożymy przez 1, dziesiątą cyfrę mnożymy przez 3,
       jedenastą cyfrę mnożymy przez 1.*/

  def validate(pesel:Long):Boolean = {

    if(pesel >= (Math.pow(10, 10)) && pesel <= (Math.pow(10,11)) ) {
      val factors = List(1, 3, 1, 9, 7, 3, 1, 9, 7, 3, 1);
      //val factors = List(1, 3, 7, 9, 1, 3, 7, 9, 1, 3, 1)
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

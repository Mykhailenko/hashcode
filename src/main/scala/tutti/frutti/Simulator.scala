package tutti.frutti

import scala.collection.mutable
;



class Simulator (val R : Long, val C : Long, val F : Long, val N : Long, val B : Long, val T : Long) {
  def simulate() = {

  }

  def maxScore() = rides.map(_.length + B).reduce(_ + _)


  var rides : mutable.MutableList[Ride] = new mutable.MutableList[Ride]

  var cars : mutable.MutableList[Car] = new mutable.MutableList[Car]

}

package tutti.frutti

import scala.collection.mutable


class Simulator(val R: Int, val C: Int, val F: Int, val N: Int, val B: Int, val T: Int) {


  var rides: mutable.MutableList[Ride] = new mutable.MutableList[Ride]

  var cars: mutable.MutableList[Car] = new mutable.MutableList[Car]

  def simulate(): Unit = {
    for (t <- 0 until T) {
      implicit val tt = t;

      while (tryAssign()) {}

      for (car <- cars) {
        car.move()
      }
    }
  }

  def tryAssign()(implicit t: Int): Boolean = {
    val freeCars = cars.filter(_.isFree())
    if (!freeCars.isEmpty && !rides.isEmpty) {
      val car = freeCars.get(0).get;

      val ride = rideForCar(car)
      if(ride.isDefined){
        car.planRides(List(ride.get));
        rides = rides diff Seq(ride.get)
        return true
      }
    }
    return false
  }

  def rideForCar(car: Car)(implicit t: Int): Option[Ride] = {
    val posibleRides = rides.map(ride => (ride, ride.computeMetrics(car.current, t, B)))
      .filter(_._2.score > 0)
    if(!posibleRides.isEmpty){
      val minTime = posibleRides.minBy(_._2.timeToStart)._2.timeToStart

      Some(posibleRides.filter(_._2.timeToStart == minTime)
        .maxBy(_._2.score)._1)
    } else {
      None
    }
  }


  def maxScore() = rides.map(_.length + B).reduce(_ + _)

  def score() = cars.map(car => {
    var time = 0
    var score = 0;
    var coord = new Coord()

    for (ride <- car.completedRides) {
      val metrics = ride.computeMetrics(coord, time, B)
      val newTime = metrics.timeNeeded
      val marginalCost = metrics.score
      coord = ride.finish
      time += newTime
      score += marginalCost
    }

    score
  }).reduce(_ + _)

}

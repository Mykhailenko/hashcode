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

      val rds = ridesForCar(car.current, t)
      if(rds.isDefined){
        car.planRides(rds.get);
//        rides = rides diff Seq(ride.get)
        return true
      }
    }


    return false
  }

  def ridesForCar(coord: Coord, currentTime: Int): Option[List[Ride]] = {
//    rides.map(ride => {
//      val m = ride.computeMetrics(coord, currentTime, B);
//      val opride2 = rideForCar(ride.finish, currentTime + m.timeNeeded)
//      if(opride2.isDefined && opride2.get.id != ride.id){
//        (Seq(ride, opride2.get), profit(coord, currentTime, Seq(ride, opride2.get)))
//      }else{
//        (Seq(ride), profit(coord, currentTime, Seq(ride)))
//      }
//    }).maxBy(_._2)
    null
  }

  def profit(from : Coord, currentTime : Int, rs : Seq[Ride]) : Int = {
    var time = currentTime
    var score = 0
    var pos = from

    for(ride <- rs){
      val m = ride.computeMetrics(pos, time, B);
      time = m.timeNeeded
      score += m.score
      pos = ride.finish
    }
    score
  }

  def rideForCar(coord: Coord, currentTime : Int): Option[Ride] = {
    val posibleRides = rides.map(ride => (ride, ride.computeMetrics(coord, currentTime, B)))
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

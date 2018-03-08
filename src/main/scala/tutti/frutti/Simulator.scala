package tutti.frutti

import scala.collection.mutable


class Simulator(val R: Int, val C: Int, val F: Int, val N: Int, val B: Int, val T: Int) {


  var rides: mutable.MutableList[Ride] = new mutable.MutableList[Ride]

  var cars: mutable.MutableList[Car] = new mutable.MutableList[Car]

  def simulate(): Unit = {
    for (t <- 0 until T) {
      println("Time " + t)
      implicit val tt = t;

      while (tryAssign()) {}

      for (car <- cars) {
        car.move()
      }
    }
  }

  def tryAssign()(implicit t: Int): Boolean = {
    println("tryAssign")
    val freeCars = cars.filter(_.isFree())
    if (!freeCars.isEmpty && !rides.isEmpty) {
      val car = freeCars.get(0).get;
      val rds = rideForCar(car, 1)
      rides = rides diff Seq(rds)
      car.planRides(rds);
      return true
    }
    return false
  }

  def rideForCar(car: Car, desiredLen: Int)(implicit t: Int): List[Ride] = {
    return rides.map(ride => {
      var ans = new mutable.MutableList[Ride]
      val first = ride
      ans += first
      var score = first.computeMetrics(car.current, t).score

      var time = t
      var coord = first.finish
      var restLen = desiredLen - 1

      while (restLen > 0 && rides.isEmpty) {
        val newR: Ride = pickRide(coord, time, ans)
        if (newR != null) {
          ans += newR
          val metr = newR.computeMetrics(coord, time, B)
          time = metr.timeNeeded
          coord = newR.finish
          score += metr.score
          restLen -= 1
        }
      }

      (ans, score)
    }).maxBy(pair => {
      pair._2
    })._1.map(identity)(collection.breakOut)
  }

  def pickRide(coord: Coord, time: Int, selected: mutable.MutableList[Ride]): Ride = {
    val left = rides.diff(Seq(selected))
    if(!left.isEmpty){
      left.map(ride => (ride, coord.dist(ride.start))).minBy(_._2)._1
    }else{
      null
    }
  }


  def seqride(pos: Coord, time: Int, addMore: Int, ridesLeft: List[Ride]): List[Ride] = {
    if (addMore > 0 && ridesLeft.size > 0) {
      val ride: Ride = null;
      ride :: seqride(pos, time, addMore - 1, ridesLeft)
    } else {
      List()
    }
  }


  def maxScore() = rides.map(_.length + B).reduce(_ + _)

  def score() = cars.map(car => {
    var time = 0
    var score = 0;
    var coord = new Coord()

    for (ride <- car.completedRides) {
      val metrics = ride.computeMetrics(coord, time)
      val newTime = metrics.timeNeeded
      val marginalCost = metrics.score
      coord = ride.finish
      time += newTime
      score += marginalCost
    }

    score
  }).reduce(_ + _)

}

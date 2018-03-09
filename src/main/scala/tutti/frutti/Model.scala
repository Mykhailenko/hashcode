package tutti.frutti

import scala.collection.mutable


case class Coord(var r: Int = 0, var c: Int = 0) {
  def dist(another: Coord) = Math.abs(r - another.r) + Math.abs(c - another.c)
}



case class CarRideMetrics(val timeNeeded: Int,
                          val timeToStart: Int,
                          val delay: Int,
                          val distanceTillStart: Int,
                          val score: Int);

case class Ride(val id: Int,
                val start: Coord,
                val finish: Coord,
                val ealiestStart: Int,
                val latestFinish: Int) {
  def length = start.dist(finish)


  def computeMetrics (from: Coord, currentTime: Int, bonus: Int) = {
    val distanceTillStart = from.dist (this.start)

    val timeToArriveToStart = currentTime + distanceTillStart;

    var delay = 0;
    var score = 0;

    if(timeToArriveToStart <= ealiestStart){
      delay = ealiestStart - timeToArriveToStart
      score += bonus
    }

    val timeToStart = distanceTillStart + delay

    val timeNeeded = timeToStart + this.length

    val finishedTime = timeNeeded + currentTime;

    if(finishedTime < latestFinish){
      score += length;
    }

    CarRideMetrics (timeNeeded, timeToStart, delay, distanceTillStart, score);
  }

}



class Car(val id: Int,
          var ride: Ride = null,
          val completedRides: mutable.MutableList[Ride] = new mutable.MutableList[Ride],
          var current: Coord = new Coord()) {

  var rideTime = 0;

  var plannedRides: mutable.Queue[Ride] = new mutable.Queue[Ride]

  def isFree() = rideTime == 0

  def planRides(rides: List[Ride])(implicit t: Int) {
    for (ride <- rides) {
      plannedRides += ride
    }
    assignRide(plannedRides.dequeue)
  }



  def assignRide(ride: Ride)(implicit t: Int) {
    this.current = ride.finish
    this.rideTime = ride.computeMetrics(this.current, t, 0).timeNeeded;
    this.ride = ride;
  }

  def move()(implicit t: Int) = {
    if (rideTime > 0) {
      rideTime -= 1
      if (rideTime == 0) {
        completedRides += ride
        ride = null
        if (!plannedRides.isEmpty) {
          assignRide(plannedRides.dequeue)
        }
      }
    }
  }

}


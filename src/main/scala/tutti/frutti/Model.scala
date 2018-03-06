package tutti.frutti;

import scala.collection.mutable
import tutti.frutti.Utils._

case class Coord(var r: Int = 0, var c: Int = 0) {
  def dist(another: Coord) = Math.abs(r - another.r) + Math.abs(c - another.c)
}

case class CarRideMetrics(val timeNeeded: Int,
                                val timeToStart: Int,
                                val delay: Int,
                                val distanceTillStart: Int,
                                val score: Int)

case class Ride(val id: Int,
           val start : Coord,
           val finish : Coord,
           val ealiestStart : Int,
           val latestFinish : Int){
  def length = start.dist(finish)

  def computeMetrics(from: Coord, startTime: Int, bonus: Int = 0) ={
    val distanceTillStart = from.dist(start)

    val timeToStart = startTime + distanceTillStart;

    val score = length + (if(timeToStart == ealiestStart) bonus else 0)

    val delay = onlyPositive(ealiestStart - timeToStart)

    val timeNeeded = timeToStart + delay + length

    CarRideMetrics(timeNeeded, timeToStart, delay, distanceTillStart, score);
  }


}


class Car(val id: Int,
          var ride: Ride = null,
          val completedRides : mutable.MutableList[Ride] = new mutable.MutableList[Ride],
          var current : Coord = new Coord()){

  var rideTime = 0;

  var plannedRides: mutable.Queue[Ride] = new mutable.Queue[Ride]

  def isFree() = rideTime == 0

  def planRides(rides: List[Ride]) {
    for(ride <- rides){
      plannedRides += ride
    }
    assignRide(plannedRides.dequeue)
  }

  def assignRide(ride : Ride)(implicit t : Int) {
    this.current = ride.finish
    this.rideTime = ride.computeMetrics(this.current, t).timeNeeded;
    this.ride = ride;
  }

  def move() = {
    if (rideTime > 0) {
      rideTime -= 1
      if(rideTime == 0){
        completedRides += ride
        ride = null
        if(!plannedRides.isEmpty){
          assignRide(plannedRides.dequeue)
        }
      }
    }
  }

}
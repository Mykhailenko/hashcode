package tutti.frutti;


import scala.collection.mutable

case class Coord(var r: Int = 0, var c: Int = 0) {
  def dist(another: Coord) = Math.abs(r - another.r) + Math.abs(c - another.c)
}

case class Ride(val id: Int,
           val start : Coord,
           val finish : Coord,
           val ealiestStart : Int,
           val latestFinish : Int){
  def length = start.dist(finish)

  def timeNeeded(cord : Coord, t: Int): Int ={
    var toStart = t + cord.dist(this.start);

    var result = if(toStart < ealiestStart){
      ealiestStart - t;
    }else{
      cord.dist(this.start);
    }

    result += this.length
    result.toInt
  }

}

class Car(val id: Int,
          private var _ride: Ride = null,
          val completedRides : mutable.MutableList[Ride] = new mutable.MutableList[Ride],
          var current : Coord = new Coord()){

  var rideTime = 0;

  def ride = _ride

  def assignRide(ride : Ride)(implicit t : Int) {
    this.current = ride.finish
    this.rideTime = ride.timeNeeded(this.current, t);
    this._ride = ride;
  }




  def move() = {
    if (rideTime > 0) {
      rideTime -= 1
      if(rideTime == 0){
        completedRides += ride
        _ride = null
      }
    }
  }

  def isFree() = rideTime == 0

}
package tutti.frutti;


import scala.collection.mutable

case class Coord(var r: Long = 0, var c: Long = 0) {
  def dist(another: Coord) = Math.abs(r - another.r) + Math.abs(c - another.c)
}

case class Ride(val id: Long,
           val start : Coord,
           val finish : Coord,
           val ealiestStart : Long,
           val latestFinish : Long){
  def length = start.dist(finish)
}

class Car(val id: Long,
          var ride: Ride = null,
          val completedRides : mutable.MutableList[Ride] = new mutable.MutableList[Ride],
          var current : Coord = new Coord()){
  def isFree() = ride == null

}
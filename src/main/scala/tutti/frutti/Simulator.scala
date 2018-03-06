package tutti.frutti

import scala.collection.mutable
;



class Simulator (val R : Int, val C : Int, val F : Int, val N : Int, val B : Int, val T : Int) {


  var rides : mutable.MutableList[Ride] = new mutable.MutableList[Ride]

  var cars : mutable.MutableList[Car] = new mutable.MutableList[Car]

  def tryAssign()(implicit t : Int): Boolean = {
    val freeCars = cars.filter(_.isFree())
    if(!freeCars.isEmpty && !rides.isEmpty){
      for(car <- freeCars){
        val ride = rideForCar(car)
        rides = rides diff Seq (ride)
        car.assignRide(ride)
        return true
      }
    }
    return false
  }


  def closest(car: Car, ride: Ride) = car.current.dist(ride.start)

  def timest(t: Int, ride: Ride) = if(t < ride.ealiestStart) ride.ealiestStart - t else 0

  def startes(car: Car, t: Int, ride: Ride) = Math.max(ride.ealiestStart - t, car.current.dist(ride.start))

  def rideForCar(car: Car)(implicit t : Int): Ride = {
      rides.map(ride => (ride,
        Prop.algo match {
          case "k" => k(car, ride)
          case "closest" => closest(car, ride)
          case "timest" => timest(t, ride)
          case "startest" => startes(car, t, ride)
        }


        ))
        .minBy(x => x._2)._1
  }

  def seqride(pos: Coord, time: Int, addMore : Int, ridesLeft : List[Ride]) : List[Ride] = {
    if(addMore > 0 && ridesLeft.size > 0) {
      val ride : Ride = null;
      ride::seqride(pos, time, addMore - 1, ridesLeft)
    } else {
      List()
    }
  }


  def timeWhenFinished


  def simulate() : Unit = {
    for(t <- 0 until T){

      implicit val tt = t;
      while(tryAssign()){}

      for(car <- cars){
        car.move()
      }
    }
  }

  def maxScore() = rides.map(_.length + B).reduce(_ + _)

  def score() = cars.map(car => {
      var time = 0
      var score = 0;
      var coord = new Coord()

      for(ride <- car.completedRides){
        val metrics = ride.computeMetrics(coord, time)
        val newTime = metrics.timeNeeded
        val marginalCost = metrics.score
        coord = ride.finish
        time += newTime
        score += marginalCost
      }

      score
    }).reduce(_ + _)


  def k(car: Car, ride: Ride)(implicit t : Int) = {
    val timeToArriveToStart = t + car.current.dist(ride.start)
    val apliedBonus = if(timeToArriveToStart == ride.ealiestStart) B else 0;
    val profit = ride.length + apliedBonus
    val delay = if(timeToArriveToStart < ride.ealiestStart)
      ride.ealiestStart - timeToArriveToStart else 0;

    Prop.k * delay - profit
  }

}

package tutti.frutti

import java.io.PrintWriter
import io._

object IO {

  val separator = " "

  def readFile(path: String): Simulator = {

    val lines = Source.fromFile(path).getLines

    val Array(r, c, f, n, b, t) = lines.next().split(separator).map(_.toInt)
    val simulator = new Simulator(r, c, f, n, b, t);

    var rideCounter = 0;
    for (line <- lines) {
      val Array(sx, sy, fx, fy, earliest, latest) = line.split(separator).map(_.toInt);
      val start = Coord(sx, sy);
      val finish = Coord(fx, fy);
      simulator.rides += Ride(rideCounter, start, finish, earliest, latest)
      rideCounter += 1;
    }

    for(i <- 0 until f){
      simulator.cars += new Car(i);
    }

    simulator
  }

  def writeToFile(path: String, simulator: Simulator): Unit = {
    val writer = new PrintWriter(path)

    for (car <- simulator.cars) {
      val sb = new StringBuffer()

      sb.append(car.completedRides.size.toString)

      for (ride <- car.completedRides) {
        sb.append(separator + ride.id)
      }

      writer.write(sb.toString + "\n")
    }

    writer.close
  }
}

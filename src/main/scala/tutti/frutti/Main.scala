package tutti.frutti;

import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val in = opt[String](required = true)
//  val out = opt[String]
  verify()
}

object Main {
  def main(args: Array[String]) {
    val cmd = new Conf(args)

    val simulator = IO.readFile(cmd.in())

    println(simulator.maxScore)

//    IO.writeToFile(cmd.out(), simulator)

  }
}
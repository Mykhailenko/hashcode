package tutti.frutti;

import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val in = opt[String](required = true)
  val out = opt[String]()
  val score = opt[Boolean]()
  val max = opt[Boolean]()
  val k = opt[Double]()
  val kmin = opt[Double]()
  val kmax = opt[Double]()
  val kstep = opt[Double]()
  val algo = opt[String]()
  verify()
}

object Main {
  def main(args: Array[String]) {
    val cmd = new Conf(args)

    if(cmd.algo.isDefined){
      Prop.algo = cmd.algo();
    }

    if(cmd.kmin.isDefined && cmd.kmax.isDefined && cmd.kstep.isDefined){
      var k = cmd.kmin();

      while(k <= cmd.kmax()){
        val simulator = IO.readFile(cmd.in())
        Prop.k = k
        simulator.simulate()
        println(k + ", " + simulator.score())
        k += cmd.kstep()
      }

    } else {
      val simulator = IO.readFile(cmd.in())
      //    println("There are cars " + simulator.cars.size)
      //    println("There are rides " + simulator.rides.size)

      if(cmd.max()){
        println("Max score " + simulator.maxScore())
      }


      if(cmd.k.isDefined){
        Prop.k = cmd.k()
      }


      simulator.simulate()


      if(cmd.k.isDefined){
        println(cmd.k() + ", " + simulator.score())
      } else if(cmd.score()){
        println(simulator.score)
      }


      if(cmd.out.isDefined){
        IO.writeToFile(cmd.out(), simulator)
      }
    }
  }
}

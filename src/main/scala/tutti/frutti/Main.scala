package tutti.frutti

import scala.language.dynamics

import org.rogach.scallop._

import scala.collection.mutable

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val in = opt[String](required = true)
  val out = opt[String]()
  val score = opt[Boolean]()
  verify()
}

object Main {



  def main(args: Array[String]) {

    class MyMap[T](defaultValue: T) extends Dynamic {

      val values: mutable.Map[String, T] = mutable.Map.empty[String, T]

      def selectDynamic(name: String): T = {
        values.get(name) match {
          case Some(value) => value
          case None => defaultValue
        }
      }

      def updateDynamic(name: String)(value: T): Unit = {
        values(name) = value
      }

    }

    val myKeyStore = new MyMap(150)

    println(myKeyStore.foo)





    val cmd = new Conf(args)

    val simulator = IO.readFile(cmd.in())

    simulator.simulate()

    if(cmd.score.isDefined){
      println(simulator.score())
    }

    if (cmd.out.isDefined) {
      IO.writeToFile(cmd.out(), simulator)
    }
  }
}

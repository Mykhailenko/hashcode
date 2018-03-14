package tutti.frutti

import java.lang.reflect.InvocationTargetException
import java.util.Date

import scala.language.dynamics

object KickDynamic {

  def main(args: Array[String]): Unit = {

    var userMap = new java.util.HashMap[String, Object]();
    userMap.put("user", "Gleb")

    println(userMap.get("user"))

    var variables = Map[String, Object]();
    variables += ("name" -> "proactive")
    //    variables += ("stockPrice" -> 1500)
    variables += ("now" -> new Date())
    variables += "userMap" -> userMap


    val wrapper = new MapWrapper(variables)

    println("'gleb'.length : " + wrapper.get("userMap").get("user").length)
    println("'gleb'.substring(1): " + wrapper.get("userMap").get("user").substring(1))
    println("'error, no substringss' : " + wrapper.get("userMap").get("user").substringss(1))
  }
}


class MapWrapper(map: Map[String, Object]) {

  def get(key: String) = {
    new VariableWrapper(map.get(key).get)
  }
}

class VariableWrapper(obj: Object) extends Dynamic {

  def applyDynamic(name: String)(args: Any*) = {

    val candidates = obj.getClass.getMethods.filter(method => method.getName.equals(name))

    val invoked = candidates.map(method => {
      var res : Object = null
      var worked = true
      try {

        res = method.invoke(obj, args.map(_.asInstanceOf[AnyRef]) : _*)
      } catch {
        case e : IllegalArgumentException => worked = false
        case e : InvocationTargetException => throw e
      }
      (res, worked)

    }).filter(pair => pair._2)
    if(!invoked.isEmpty){
      new VariableWrapper(invoked.head._1)
    } else{
      throw new NoSuchMethodException(s"Object $obj does not have method $name which takes: "
        + args.map(_.toString).mkString(" "))
    }

  }

  override def toString() = {
    obj.toString
  }

  def selectDynamic(name: String) = {
    obj.getClass.getMethod(name).invoke(obj)
  }
}
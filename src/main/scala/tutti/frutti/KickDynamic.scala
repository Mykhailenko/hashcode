package tutti.frutti

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

    println("Name length " + wrapper.get("userMap").get("user").length)

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

    val res = candidates.map(method => {
      var res = null
      var worked = true
      try{
        res = method.invoke(obj, args)
      }catch(IllegalAccessException e){
        worked = false
      }
      (res, worked)

    }).filter(pair => pair._2).head._1

    new VariableWrapper(res)
  }

  override def toString() = {
    obj.toString
  }

  def selectDynamic(name: String) = {
    obj.getClass.getMethod(name).invoke(obj)
  }
}
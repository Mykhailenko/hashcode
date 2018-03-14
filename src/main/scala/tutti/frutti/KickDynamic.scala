package tutti.frutti

import java.lang.reflect.InvocationTargetException
import java.util.Date

import scala.language.dynamics

object KickDynamic {

  implicit def toDouble (dyn : DynamicWrapper) : Double = {
    dyn.value.asInstanceOf[Any] match {
      case d : Double => d.toDouble
      case f : Float => f.toDouble
      case l : Long => l.toDouble
      case i: Int  => i.toDouble
      case s : Short => s.toDouble
      case b : Byte => b.toDouble
    }
  }


  def main(args: Array[String]): Unit = {

    var javaMap = new java.util.HashMap[String, Object]();
    javaMap.put("user", "Gleb")

    var variables = Map[String, Object]();
    variables += ("name" -> "proactive")
    variables += ("now" -> new Date())
    variables += "javaMap" -> javaMap



    val wrapper = new DynamicWrapper(variables)

    println("'gleb'.length : " + wrapper.get("javaMap").get.get("user").length)

    val len = wrapper.get("javaMap").get.get("user").length


    println(5 + len)

    println("6.5" + len)

    println("'gleb'.substring(1): " + wrapper.get("javaMap").get.get("user").substring(1))
    println("'error, no substringss' : " + wrapper.get("javaMap").get.get("user").substringss(1))
  }
}

class DynamicWrapper(obj: Object) extends Dynamic {

  def value = obj



  def applyDynamic(name: String)(args: Any*) : DynamicWrapper = {

    val candidates = obj.getClass.getMethods.filter(method => method.getName.equals(name))

    candidates.foreach(method => {
      try {
        if(args.size != 0){
          return new DynamicWrapper(method.invoke(obj, args.map(_.asInstanceOf[AnyRef]): _*))
        }else{
          return new DynamicWrapper(method.invoke(obj))
        }
      } catch {
        case e: IllegalArgumentException => {}
        case e: InvocationTargetException => throw e.getCause
        case e: Exception => throw e
      }
    })

    throw new NoSuchMethodException(s"Object $obj does not have method $name which takes: "
      + args.map(_.toString).mkString(" "))
  }

  def selectDynamic(name: String) : DynamicWrapper = applyDynamic(name)()

  override def toString: String = obj.toString

}
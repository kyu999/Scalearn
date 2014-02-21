package datastore
import scala.util.Random._
object DataTes extends App{
  
  val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
  val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
  val a=(1 to 10000).map(in=>nextDouble).toVector
  val b=(1 to 10000).map(in=>nextDouble).toVector
  
  data(x)
  dataset(x,y)
  

 // dataset(a,b)
 
  
  
  
}
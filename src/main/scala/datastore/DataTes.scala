package datastore
import scala.util.Random._
object DataTes extends App{
  
  val x=Stream(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
  val y=Stream(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
  val a=(1 to 1000).map(in=>nextDouble).toStream
  val b=(1 to 1000).map(in=>nextDouble).toStream
  
  data(x)
  dataset(x,y)
  

//  val EnorData=Dataset(a,b)
 
  
  
  
}
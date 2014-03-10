package factorytest

import org.scalatest.FunSuite
import datafactory._
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import Converter._

class InferenceTest extends FunSuite{
	
    val x=Vector(120,135,116,132,124,130.0)
    val y=Vector(106.0,113,98,114,107,120)
    val a=(1 to 10000).map(in=>nextDouble).toVector
    val b=Vector(3,4,5,6)

    val dx=x.toda
    val dy=data(y)
    val dsxy=Vector(x,y).tods		
    val db=b.toda
    val dsxyb=dase(dx,dy,db) 
    
    test("paired-t"){
      println("paired_t_test"+dsxy.tpair)
    }
}
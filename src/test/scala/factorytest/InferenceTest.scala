package factorytest

import org.scalatest.FunSuite
import datafactory._
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import Converter._

class InferenceTest extends FunSuite{
	
    val x=Vector(90.0,75,75,75,80,65,75,80)
    val y=Vector(95.0,80,80,80,75,75,80,85)
    val a=(1 to 10000).map(in=>nextDouble).toVector
    val b=Vector(3,4,5,6)
    val s=Vector(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30) 
    val t=Vector(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29) 
    val m=(0 to 10).map(_.toDouble).toVector
    val l=(0 to 100).map(_.toDouble).toVector
    
    val dx=x.toda
    val dy=data(y)
    val db=b.toda
    val ds=s.toda
    val dt=t.toda
    val dm=m.toda
    val dl=l.toda
    
    val dsxy=Vector(x,y).tods		
    val dsxyb=dase(dx,dy,db) 
    val dsst=dase(ds,dt)
    val dsml=dase(dm,dl)
    
    
    test("paired-t"){
      assert((-2.965614910077132,false)==dsxy.tpair.head)
      assert((3.0353754156485913,false)==dsst.tpair.head)
    }
    test("welch_t_test"){
      assert(dsxy.twelch.head==(-1.299867367239363,true))
      assert(dsml.twelch.head==(-14.599927901768629,false))
    }
}
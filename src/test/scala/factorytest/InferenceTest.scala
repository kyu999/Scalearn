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
    
    val dx:data=x.toda
    val dy:data=data(y)
    val db:data=b.toda
    val ds:data=s.toda
    val dt:data=t.toda
    val dm:data=m.toda
    val dl:data=l.toda
    
    val dsxy:infds=Vector(x,y).toinf		
    val dsxyb:infds=infds(dx,dy,db) 
    val dsst:infds=infds(ds,dt)
    val dsml:infds=infds(dm,dl)
    
    
    
    test("paired-t"){
      assert((-2.965614910077132,false)==dsxy.tpair.head)
      assert((3.0353754156485913,false)==dsst.tpair.head)
    }
    
    test("welch_t_test"){
      assert(dsxy.twelch.head==(-1.299867367239363,true))
      assert(dsml.twelch.head==(-14.599927901768629,false))
    }
    
    val infds1=Vector(
    				Vector(77.4,78.2,78.1,77.8,77.9),
    				Vector(78.3,78.2,78.4,77.3,79.1),
    				Vector(79.2,79.3,79.1,78.2,79.3),
    				Vector(78.9,78.8,78.1,78.1,78.9)).toinf
    				
    test("anova"){
      println("grand sum : "+infds1.grandsum)
      println("grand size : "+infds1.grandsize)
      println("grand mean : "+infds1.grandmean)
      println("factor df : "+infds1.factorDf)
      println("error df : "+infds1.errorDf)
      println("factor SS : "+infds1.factorSS)
      println("error SS : "+infds1.errorSS)
      println("factor MS : "+infds1.factorMS)
      println("error MS : "+infds1.errorMS)
      println("F : "+infds1.f)
    }
}
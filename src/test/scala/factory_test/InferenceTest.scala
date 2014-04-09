package factory_test

import org.scalatest.FunSuite
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import scala.collection.mutable.ListBuffer
import org.scalautils.TolerantNumerics._

import scalearn.statistics._
import scalearn.statistics.Converter._


class InferenceTest extends FunSuite{
	
  
	implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	

	
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
    
    val infds1=Vector(
    				Vector(77.4,78.2,78.1,77.8,77.9),
    				Vector(78.3,78.2,78.4,77.3,79.1),
    				Vector(79.2,79.3,79.1,78.2,79.3),
    				Vector(78.9,78.8,78.1,78.1,78.9)).toinf
   
    
    test("paired-t"){
      
      assert((false,-2.965614910077132)==dsxy.tpair.head)
      assert((false,3.0353754156485913)==dsst.tpair.head)
      
    }
    
    test("welch_t_test"){
      
      assert(dsxy.twelch.head==(true,-1.299867367239363))
      assert(dsml.twelch.head==(false,-14.599927901768629))
      
    }
    
    test("F test"){
      val result=Vector(Vector(1,2,3,555,666,777,4,5,654,34,3.9),Vector(1,2,3,4,5,6,7,8,9000.0)).toinf.ftest.head
      assert(false==result._1)
      assert(80.3401===result._2)
      
    }
    
        				
    test("subtotaling & anova"){

      assert(
    		  infds1.subtotaling(Vector(Vector(1,2,3),Vector(4,5,6),Vector(7,8,9)))
    		  ==ListBuffer(12.0, 15.0, 18.0))
    		  
      assert((false,5.130018416218995)==infds1.anova)
      

    }
}
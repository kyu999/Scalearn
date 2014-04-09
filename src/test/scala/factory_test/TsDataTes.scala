package factory_test

import org.scalatest.FunSuite
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import scalearn.statistics._
import scalearn.statistics.Converter._

class TsdaTes extends FunSuite {
    
	implicit val doubleEquality = tolerantDoubleEquality(0.01)	  	
    	 
	val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val z=Vector(1.0,2,5,4,3)
 
	val da1=Vector(5,8,6,9,10.0,11).toda.tots
    val d1=x.toda 
	val ts1=data(z)
     
    test("autocovariance"){
	  
      assert(ts1.tots.autocovariance(ts1.raw, -1)===2.0,"autocov:lag=-1; of course, this is only for weired input") 
//      assert(ts1.tots.autocovariance(ts1.raw, 0)===2.0,"autocov:lag=0") 
      assert(ts1.tots.autocovariance(ts1.raw, 1)===0.4,"autocov:lag=1") 
      assert(ts1.tots.autocovariance(ts1.raw, 2)===(-1.0),"autocov:lag=2") 
      assert(ts1.tots.autocovariance(ts1.raw, 3)===(-0.4),"autocov:lag=3") 
      assert(ts1.tots.autocovariance(ts1.raw, 5)===0.0,"autocov:lag=4") 
      val zipped=d1.tots.acov.zip(Vector(315.20,-92.52,-65.49,125.25,-136.17,43.99,40.18,-86.68,69.38,-4.43,-69.63,14.03))
      zipped.foreach(a=>assert(a._1===a._2))
    }
	
    test("autocorrelation"){
       
      assert(ts1.tots.acf._2===Vector(1.0, 0.2, -0.5, -0.2, 0.0, 0.0),"acf fail")
      assert(d1.tots.acf._2===Vector(1.0, -0.29351153863941176, -0.20777412939925083, 0.3973546686398743, -0.43199371040096196, 0.13956435277251078, 0.12747074873976785, -0.27500809323405634, 0.22011746751144617, -0.014054479026962028, -0.22090366739120382, 0.04450353789945891, 0.018748554779632807, -0.004513712250844008, 0.0)
    		  		,"acf fail")
    		  		
    }
    
    test("detrending(residual)"){
      
	  assert(da1.resi.raw(0)===(-0.38095238))
	  assert(da1.resi.raw(1)===(1.50476190))
	  assert(da1.resi.raw(2)===(-1.60952381))
	  assert(da1.resi.raw(3)===(0.27619048))
	  assert(da1.resi.raw(4)===(0.16190476))
	  assert(da1.resi.raw(5)===(0.04761905))	      
//how to examine collection da?
	  
    }

	test("differencing"){
	  
	  assert(da1.differencing(da1.raw)===Seq(3,-2,3,1,1.0))

	  
	}
	
	test("partial auto correlation"){
//	  println("peason bug : "+ts1.pearRaw(Vector(1.0), Vector(2.0)))
//	  println("pacf for ts1 : "+ts1.ts.pacf)
	  
	}
}
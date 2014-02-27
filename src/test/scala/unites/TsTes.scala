package unites
import org.scalatest.FunSuite
import datafactory._
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import Converter._

class TsTes extends FunSuite {
    
	implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	
    	 
	val da1=List(5,8,6,9,10.0,11).toda.ts
	val time=(1 to da1.n).map(_.toDouble).toList
	
    test("detrending(residual)"){
	  assert(da1.resi(0)===(-0.38095238))
	  assert(da1.resi(1)===(1.50476190))
	  assert(da1.resi(2)===(-1.60952381))
	  assert(da1.resi(3)===(0.27619048))
	  assert(da1.resi(4)===(0.16190476))
	  assert(da1.resi(5)===(0.04761905))	      
//how to examine collection data?
    }

	test("differencing"){
	  assert(da1.differencing(da1.raw)===Seq(3,-2,3,1,1.0))
	}
}
package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datafactory._
class MatrixTes extends FunSuite{
    val r1=Vector(Vector(1,2.0,3),Vector(4,5,6.0),Vector(7,8,9.0))
    val r2=r1
	val m1=new matrix(r1)
	test("preprocessing"){
	  m1.matcul(r1, r2)
	}
}
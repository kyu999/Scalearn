package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datafactory._
import Converter._ 
import math._
import scala.collection.immutable.{Vector=>v}
class MatrixTes extends FunSuite{
  
	val a=v(1,2.0,3)
	val b=v(4,5,6.0)
	val c=v(7,8,9.0)
	
	val v1=(1 to 100000).map(_.toDouble).toVector
	val m1=m(a,b,c)
	
    val v2=v(v1,v1,v1,v1)
    val m2=new m(v2)

	val m3=m1.t
	
    test("+"){
	  assert( (m1+m1).raw == v(v(2,4,6),v(8,10,12),v(14,16,18)))
//	  m1+m2 should return error 
	}
	
	test("-"){
	  assert( (m1-m1).raw == v(v(0,0,0),v(0,0,0),v(0,0,0)))
	}
	
	test("*"){
	  assert( (m(v(2,-1.0),v(-3,4))*(m(v(1,2)).t)).raw == (m(v(0,5.0)).t).raw )
	  assert( (m(v(1,-1),v(-2,3))*m(v(1,2),v(3,4))).raw == (m(v(-2,-2),v(7,8))).raw )
    }
	
	test("transepose"){
	  assert( m1.t.raw == v(v(1,4,7),v(2,5,8),v(3,6,9)))
	}
	
    test("preprocessing"){	  
	  (m2*m2.t).raw.foreach(println)
//	  m2.matcul(r4,m2.temp).foreach(println)
	  //10万要素だと7秒かかる。これいかに？
	}
}
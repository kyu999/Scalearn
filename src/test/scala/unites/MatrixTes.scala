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
	
	val v1=(1 to 10).map(_.toDouble).toVector
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
	  assert( (m1-m1+m1-m1+m1).raw == m1.raw )
	}
	
	test("matrix*matrix"){
	  assert( (m(v(2,-1.0),v(-3,4))*(m(v(1,2)).t)).raw == (m(v(0,5.0)).t).raw )
	  assert( (m(v(1,-1),v(-2,3))*m(v(1,2),v(3,4))).raw == (m(v(-2,-2),v(7,8))).raw )
    }
	
	test("scale**matrix"){
	  assert ( (m1**3).raw == Vector(Vector(3.0, 6.0, 9.0), Vector(12.0, 15.0, 18.0), Vector(21.0, 24.0, 27.0)) )
	  assert ((10**m1).raw == Vector(Vector(10.0, 20.0, 30.0), Vector(40.0, 50.0, 60.0), Vector(70.0, 80.0, 90.0)))
	  assert( (m1+m1+m1).raw == (3**m1).raw )
	}
	
	test("transepose"){
	  assert( m1.t.raw == v(v(1,4,7),v(2,5,8),v(3,6,9)))
	}
	
    test("preprocessing"){	  
	  (m2*m2.t).raw.foreach(println)
	  //100万要素だと7秒かかる。これいかに？
	}
}
package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datafactory._
import Converter._ 
import math._
class MatrixTes extends FunSuite{
  
	val a=Vector(1,2.0,3)
	val b=Vector(4,5,6.0)
	val c=Vector(7,8,9.0)
	
	val v1=(1 to 100000).map(_.toDouble).toVector
	val m1=m(a,b,c)
	
    val v2=Vector(v1,v1,v1,v1)
    val m2=new m(v2)

	val m3=m1.T

    test("preprocessing"){
	  println(m1*m1)
	  println(m1*m3)
//	  v2.foreach(println)
	  (m2*m2).foreach(println)
//	  m2.matcul(r4,m2.temp).foreach(println)
	  //10万要素だと7秒かかる。これいかに？
	}
}
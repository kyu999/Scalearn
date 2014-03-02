package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datafactory._
import Converter._
class BreMatrixTes extends FunSuite{
	val v1=(1 to 100).map(_.toDouble).toVector
    val r1=Vector(Vector(1,2.0,3),Vector(4,5,6.0),Vector(7,8,9.0))
    val r2=r1
	val m1=new matrix(r1)
    val r3=m1.temp
    val r4=Vector(v1,v1,v1,v1)
    val m2=new matrix(r4)
	val m3=r4.tods.tomat		//datasetをmatrixへ変換

    test("preprocessing"){
	  m1.matcul(r1, r2)
//	  println(m1.temp)
	  m1.matcul(r1,r3)
//	  m2.matcul(r4,m2.temp).foreach(println)
	  //10万要素だと7秒かかる。これいかに？
	}
}
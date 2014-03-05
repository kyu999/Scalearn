package unites
import org.scalatest.FunSuite	
import datafactory._
import scala.util.Random.nextDouble
//import org.scalautils.Equality
import org.scalautils.TolerantNumerics._
//Doubleの丸め誤差の許可範囲設定のためのやつ
import Converter._

class DataTes extends FunSuite {

    implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	
	//===の誤算範囲を上書き設定。小数点三桁以下まで許可

    val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
    val z=Vector(1.0,2,5,4,3)
    val a=(1 to 10000).map(in=>nextDouble).toVector
  
    val d1=x.toda
    val d2=data(y)
    val d3=z.toda
    val d4=a.toda
    
    //このDataTesとTsDataTesを重点的にすることが何より大事！！
    
    test("mean"){ 
	  assert(2.0009===2.0)	//===を使えば小数点三桁以下までの誤差は認めてくれる。
	  assert(d1.mean===43.714," -> mean")
	  assert(d2.mean===46.571," ->mean")
	}
    test("sd"){ 
	  assert(d1.sd===18.424," -> sd")
	  assert(d2.sd===8.933," ->sd")
	}	
    test("regression"){
   	  assert(d1.regline(10)===46.208)
    }
    test("time"){
      assert(d1.time===(1 to x.length))
      assert(d2.time===(1 to y.length))
      assert(d3.time===(1 to z.length))
      assert(d4.time===(1 to a.length))
    }
    

      
}
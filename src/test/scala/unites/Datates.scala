package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datastore._
import scala.util.Random.nextDouble
import org.scalautils.Equality

import org.scalautils.TolerantNumerics._
//Doubleの丸め誤差の許可範囲設定のためのやつ

class Datates extends FunSuite {
   
	implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	
	//===の誤算範囲を上書き設定。小数点三桁以下まで許可
    
    val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
    val a=(1 to 10000).map(in=>nextDouble).toVector
    val b=(1 to 10000).map(in=>nextDouble).toVector
    
    val d1=data(x)
    val d2=data(y)
    val d3=dataset(x,y)

    test("data class : mean & sd"){ 
	  assert(2.0009===2.0)	//===を使えば小数点三桁以下までの誤差は認めてくれる。
	  assert(d1.mean===43.714," -> mean")
	  assert(d1.sd===18.424," -> sd")
	  assert(d2.mean===46.571," ->mean")
	  assert(d2.sd===8.933," ->sd")
	}
    
    test("dataset class : cov , pear , spear"){
      assert(d3.cov===(-154.901)," -> cov")
      assert(d3.pear===(-0.941)," -> pear")
      assert(d3.spcor===(-0.896)," -> spear") 
    }
}
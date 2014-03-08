package factorytest
import org.scalatest.FunSuite	
import datafactory._
import scala.util.Random.nextDouble
//import org.scalautils.Equality
import org.scalautils.TolerantNumerics._
//Doubleの丸め誤差の許可範囲設定のためのやつ
import Converter._

class daTes extends FunSuite {

    implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	
	//===の誤算範囲を上書き設定。小数点三桁以下まで許可

    val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
    val z=Vector(1,2,5,4,3)
    val a=(1 to 10000).map(in=>nextDouble).toVector
    val b=Vector(4,5,6,4,3,2,5,6,7,5,433,3,3,9,8,96,56,4,33,2)
  
    val d1=x.toda
    val d2=da(y)
    val d3=z.toda
    val d4=a.toda
    val d5=b.toda
    
    //このdaTesとTsdaTesを重点的にすることが何より大事！！
    
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
   	  assert(d5.reg._1===1.947)
   	  assert(d5.reg._2===14.252)
    }
    test("residual"){
      assert(d5.resi===Vector(-12.2, -13.147368421052633, -14.094736842105263, -18.042105263157897, -20.989473684210527, -23.93684210526316, -22.88421052631579, -23.83157894736842, -24.778947368421054, -28.726315789473688, 397.3263157894737, -34.62105263157895, -36.56842105263158, -32.51578947368421, -35.463157894736845, 50.589473684210525, 8.642105263157887, -45.30526315789474, -18.252631578947373, -51.2))
    }
    test("time"){
      assert(d1.time===(1 to x.length))
      assert(d2.time===(1 to y.length))
      assert(d3.time===(1 to z.length))
      assert(d4.time===(1 to a.length))
    }
    

      
}
package unites
import org.scalatest.FunSuite	//継承するFunSuiteトレイト
import datafactory._
import scala.util.Random.nextDouble
//import org.scalautils.Equality
import org.scalautils.TolerantNumerics._
//Doubleの丸め誤差の許可範囲設定のためのやつ
import Converter._

class Datates extends FunSuite {

    implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	
	//===の誤算範囲を上書き設定。小数点三桁以下まで許可

    val x=List(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
    val z=Stream(1.0,2,5,4,3)
    val a=(1 to 10000).map(in=>nextDouble).toSeq
    val b=(1 to 10000).map(in=>nextDouble).toSeq
    val c=(1 to 30).map(in=>in.toDouble).toSeq
  // List(1).toda
    val d1=x.toda
    val d2=data(y)
    val d3=Stream(x,y).tods.ts		//通常のStreamをtodsでdataset化し、tsで時系列データ化
    val enor1=List(a,b,a,b).tods
    val d4=dataset(d1,d2,List(3,4,5,6.0).toda)	//rawかdataどちらかを突っ込めるようにしたい=>dataのみ許可
    val dataindataset=dataset(d1,d2)
    val cons=d1::d2
    cons.summary
    (d1::d2::d2).summary		
    //こんな感じでcons(::)を使ってdatasetを作ることも可能。だが効率上あまり好ましくない。基本的にはdataset(d1,d2,d2)の方が良い
    
    test("data class : mean"){ 
	  assert(2.0009===2.0)	//===を使えば小数点三桁以下までの誤差は認めてくれる。
	  assert(d1.mean===43.714," -> mean")
	  assert(d2.mean===46.571," ->mean")
	}
    test("data class : sd"){ 
	  assert(d1.sd===18.424," -> sd")
	  assert(d2.sd===8.933," ->sd")
	}	
    test("data class : regression"){
//    	  assert(d1.reg(10)===46.0)
    }
    
    test("dataset class : cov"){
      assert(d3.covar(0)===(-154.901)," -> cov")
    }
    test("dataset class : pear"){
      assert(d3.pears(0)===(-0.941)," -> pear")
    }
    test("dataset class : spear"){
      assert(d3.spears(0)===(-0.896)," -> spear") 
    }
    test("dataset class : regression"){
      val target=dataset(d2,d1)
      assert(target.xregline(0)(10)===114.701)
      assert(target.yregline(0)(114.701)===10.000)
    }

    
    val ts1=data(z)
    val ts2=data(c)
     
    test("time series trait : autocovariance"){
      assert(ts1.ts.autocovariance(ts1.raw, -1)===2.0,"autocov:lag=-1; of course, this is only for weired input") 
      assert(ts1.ts.autocovariance(ts1.raw, 0)===2.0,"autocov:lag=0") 
      assert(ts1.ts.autocovariance(ts1.raw, 1)===0.4,"autocov:lag=1") 
      assert(ts1.ts.autocovariance(ts1.raw, 2)===(-1.0),"autocov:lag=2") 
      assert(ts1.ts.autocovariance(ts1.raw, 3)===(-0.4),"autocov:lag=3") 
      assert(ts1.ts.autocovariance(ts1.raw, 5)===0.0,"autocov:lag=4") 
    }
    test("time series trait : autocorrelation"){
      assert(ts1.ts.acf===Vector(1.0, 0.2, -0.5, -0.2, 0.0, 0.0),"acf fail")
      assert(d1.ts.acf===Vector(1.0, -0.29351153863941176, -0.20777412939925083, 0.3973546686398743, -0.43199371040096196, 0.13956435277251078, 0.12747074873976785, -0.27500809323405634, 0.22011746751144617, -0.014054479026962028, -0.22090366739120382, 0.04450353789945891, 0.018748554779632807, -0.004513712250844008, 0.0)
    		  		,"acf fail")
    }
    
    test("side effect"){
      d4.naming("you are the winner!!","Don't get lost","I wanna be like him","This does not appear")
      d4.summary
    }
      
}
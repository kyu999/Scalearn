package unites
import org.scalatest.FunSuite	
import classifier._
import datafactory._
import Converter._
import scala.math._

class LinearRegressionTes extends FunSuite{
  
	val lr=new linreg
	val raw=(1 to 100).map(_.toDouble).toVector
	val x=(1 to raw.length).map(a=>a.toDouble).toVector
	val d1=raw.toda
	
	test("BatchGradientDescent"){
//	  val ans=lr.BatchGradientDescent(x, raw, 0.0001,2.8,1.0)
//	  assert( abs(ans._1 - d1.reg._2 ) < 0.1 && abs(ans._2 - d1.reg._1 ) < 0.1 )
	}
	//θ0にあまりにも桁の違いすぎる値を入れると、iterateが間に合わずちゃんと収束しない。αが大きすぎても収束しない。
	//αが小さすぎると時間がかかる.reg関数は(slope,intercept)だけどどBatchは(intercept,slope)==(θ0,θ1)
}
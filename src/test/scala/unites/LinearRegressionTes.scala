package unites
import org.scalatest.FunSuite	
import classifier._
class LinearRegressionTes extends FunSuite{
	val lr=new linreg
	val raw=(1 to 100).map(_.toDouble).toVector
	val x=(1 to raw.length).map(a=>a.toDouble).toVector
	lr.BatchGradientDescent(x, raw, 0.0001,20.8,100.9)
	//θ0にあまりにも桁の違いすぎる値を入れると、iterateが間に合わずちゃんと収束しない。αが大きすぎても収束しない。αが小さすぎると時間がかかる
}
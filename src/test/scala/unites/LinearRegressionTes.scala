package unites
import org.scalatest.FunSuite	
import classifier._
class LinearRegressionTes extends FunSuite{
	val lr=new linreg
	val raw=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
	val x=(1 to raw.length).map(a=>a.toDouble).toVector
	lr.BatchGradientDescent(x, raw, 0.000001)
}
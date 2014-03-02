package unites
import io._
import datafactory._
import Converter._
import org.scalatest.FunSuite	

class IoTes extends FunSuite{
	test("read"){
	  val r1=read.csv("resource/earthquake.csv")(false)
//	  r1.raw.filter(a=>a(0)>1950).foreach(println)
	}
}
package factory_test
import io._
import datafactory._
import Converter._
import org.scalatest.FunSuite	

class IoTes extends FunSuite{
	test("read"){
	  val r1=read.csv("resource/download.csv")(false)
//	  r1(2).filter(_.toDouble>1)
	}
}
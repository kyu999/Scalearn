package factory_test

import org.scalatest.FunSuite
import datafactory._
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import Converter._

class TsdsTes extends FunSuite {

  implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	

  //Entity
  val ts1=Vector(3.4,5,2,5,7,95,31,3,57,4,7,4,63,1,1,111,5).toda.tots
  val ts2=Vector(7,6,457,4,7,6,78,9,9,97,6,5,76,4,3,3.6,4).toda.tots
  val ts3=Vector(5,4,3,25,5.9).toda.tots
  val tsds1=tsds(ts1,ts2,ts3)
  //暗黙の型変換を定義する必要あり。Seq[tsda].totsds(もしくはtods)=>tsds(....)へと変換
  
  
  test("acf"){
    assert(tsds1.acf(0)===ts1.acf)
    assert(tsds1.acf(1)===ts2.acf)
    assert(tsds1.acf(2)===ts3.acf)
  }
  
  test("diff"){
    assert(tsds1.differencing.map(elt=>elt.raw)===Seq(ts1.differencing.raw,ts2.differencing.raw,ts3.differencing.raw))
  }
}
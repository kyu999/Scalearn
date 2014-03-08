package factorytest

import org.scalatest.FunSuite
import datafactory._
import scala.util.Random.nextDouble
import org.scalautils.TolerantNumerics._
import Converter._
import scala.collection.mutable.ArrayBuffer

class dsTes extends FunSuite {
  
  implicit val doubleEquality = tolerantDoubleEquality(0.001)	  	

    val x=Vector(35.0,20,63,59,14,44,42,25,73,38,56,69,28,46)
    val y=Vector(47.0,62,36,40,58,46,50,57,38,44,40,32,54,48)
    val a=(1 to 10000).map(in=>nextDouble).toVector
    val b=Vector(3,4,5,6.0)

    val d1=x.toda
    val d2=da(y)
    val d3=Vector(x,y).tods		
    val s1=b.toda
    val d4=ds(d1,d2,s1) 
    val cons=(d1::d2::d2)
    val enor1=Vector(a,b,x,y).tods


    test("cov"){
    
      assert(d3.covar(0)===(-154.901)," -> cov")
      
    }
  
    test("pear"){
      
      assert(d3.pears(0)===(-0.941)," -> pear")
      
    }
    
    test("spear"){
      
      assert(d3.spears(0)===(-0.896)," -> spear") 
      
    }
    
    test("regression"){
      
      val target=ds(d2,d1)
      assert(target.xregline(0)(10)===114.701)
      assert(target.yregline(0)(114.701)===10.000)
      assert(target.reg(0)==target.regRaw(target.raw(0),target.raw(1)))
      
    }

    test("time"){
      
      assert(d4.time===Vector(d1.time,d2.time,s1.time))
      
      
    }
    
    test("side effect"){
      val resolved=d4.resolve
      d4.naming("you are the winner!!","Don't get lost","I wanna be like him","This does not appear")
      assert(resolved(0).name=="you are the winner!!")
      assert(resolved(1).name=="Don't get lost")
      assert(resolved(2).name=="I wanna be like him")
      
    }
    
    test("basic operation"){
      
      (d1::d2).raw.zip(ds(d1,d2).raw).foreach(a=>assert(a._1==a._2))
      (d1::d2::s1).raw.zip(ds(d1,d2,s1).raw).foreach(a=>assert(a._1===a._2))
      
    }
    
}
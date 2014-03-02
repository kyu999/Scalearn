package datafactory
import Converter._
class data(x:Vector[Double]) extends Descritive{
//Descriptive
    var name:String="data"		
           
 //２つはmutable   
    
    val n=x.length
    
    val raw:Vector[Double]=x 
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)
	
    val sd=stdevi(devito2(dv))
    
    lazy val time:Vector[Double]=(1 to x.length).map(a=>a.toDouble).toVector
    
    lazy val reg:(Double,Double)=dataset(time.toda,this).reg(0)		 //x軸が時間軸のケース。
    
    lazy val regline:Double=>Double = regressionline(reg._1,reg._2)
        
    lazy val resi:Vector[Double]=residual(time,raw,regline)

    
    //Operation-------------------------------------------
    
    def ts=new tsdata(x)
    //時系列データ化
    
    def ::(component:data)=dataset(component,this)
    //２つのdataを１つのdatasetにする
        
	def summary={ 
	    Vector(name+" : "+raw,"length : "+n,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
}

//コンパニオンオブジェクト
object data{
  def apply(x:Vector[Double])=new data(x)
  //方向を暗黙的引数として入れる必要あり
}
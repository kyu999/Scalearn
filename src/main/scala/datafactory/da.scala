package datafactory
import Converter._
class da(x:Seq[Double]) extends Descritive{
//Descriptive
    var name:String="da"		
           
 //２つはmutable   
    
    val n=x.length
    
    val raw=x.toVector
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)
	
    val sd=stdevi(devito2(dv))
    
    lazy val time:Seq[Double]=(1 to x.length).map(a=>a.toDouble).toSeq
    
    lazy val reg:(Double,Double)=regRaw(time,raw)		 //x軸が時間軸のケース。
    
    lazy val regline:Double=>Double = regressionline(reg._1,reg._2)
        
    lazy val resi:Seq[Double]=residual(time,raw,regline)

    
    //Operation-------------------------------------------
    
    def ts=new tsda(x) 
    //時系列データ化
    
    def ::(component:da)=ds(component,this)
    //２つのdaを１つのdsにする
        
	def summary={ 
	    Seq(name+" : "+raw,"length : "+n,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
}

//コンパニオンオブジェクト
object da{
  def apply(x:Seq[Double])=new da(x)
  //方向を暗黙的引数として入れる必要あり
}
package datafactory
import Converter._
class data(x:Seq[Double]) extends Descritive{
//Descriptive
    var name:String="data"		//mutable
      
    var direction=true		//true=>縦、false=>横  ; mutable
      
    val n=x.length
    
    val raw:Seq[Double]=x 
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)
	
    val sd=stdevi(devito2(dv))
    
    lazy val time:IndexedSeq[Double]=(1 to x.length).map(a=>a.toDouble)
    
    lazy val reg:(Double,Double)=dataset(time.toda,this).reg(0)		 //x軸が時間軸のケース。
    
    lazy val regline:Double=>Double = regressionline(reg._1,reg._2)
        
    lazy val resi:Seq[Double]=residual(time,raw,regline)

    
    //Operation-------------------------------------------
    
    def ts=new tsdata(x)
    //時系列データ化
    
    def ::(component:data)=dataset(component,this)
    //２つのdataを１つのdatasetにする
    
	def summary={ 
	    Seq(name+" : "+raw,"length : "+n,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
}

//コンパニオンオブジェクト
object data{
  def apply(x:Seq[Double])=new data(x)
}
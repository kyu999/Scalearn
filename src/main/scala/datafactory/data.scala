package datafactory
import Converter._
class data(x:Seq[Double]) extends Descritive{
//Descriptive
    var name:Any="data"		//mutable
      
    val n=x.length
    
    val raw:Seq[Double]=x 
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)
	
    val sd=stdevi(devito2(dv))
    
    lazy val time:IndexedSeq[Double]=(1 to x.length).map(a=>a.toDouble)
    
    lazy val reg:(Double,Double)=dataset(time.toda,this).reg(0)		 //x軸が時間軸のケース。
    
    lazy val xregline:Double=>Double = XtoYregline(reg._1,reg._2)
    
    lazy val yregline:Double=>Double = YtoXregline(reg._1,reg._2)
    
    lazy val resi:Seq[Double]=residual(time,raw,xregline)

    
    //Operation-------------------------------------------
    
    def ts=tsdata(x)
    //時系列データ化
    
    def ::(component:data)=dataset(this,component)
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
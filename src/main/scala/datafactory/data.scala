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
    lazy val reg=dataset(this,(0 to x.length).map(a=>a.toDouble).toda).reg(0)		 //x軸は時間軸。
    
    
//Operation
    
    def ts=tsdata(x)
    
 //   def ::(component:data)=dataset(x,component.raw)
    
	def summary={ 
	    Seq(name+" : "+raw,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
}

//コンパニオンオブジェクト
object data{
  def apply(x:Seq[Double])=new data(x)
}
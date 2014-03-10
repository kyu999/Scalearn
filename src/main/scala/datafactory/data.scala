package datafactory
import Converter._
class data(x:Vector[Double]) extends Descritive{
  
//Descriptive
  
    var name:String="data"		       
    	//mutable   
    
    val n=x.length
    
    val raw=x.toVector
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)
	
    val sd=stdevi(devito2(dv))
    //データを標本と見なし不偏分散で出している。通常の分散が必要ならpopstdeviメソッドを使うべし
    
    lazy val popsd=popstdevi(devito2(dv))
    
    lazy val time:Vector[Double]=(1 to x.length).map(a=>a.toDouble).toVector
    
    lazy val reg:(Double,Double)=regRaw(time,raw)		 //x軸が時間軸のケース。
    
    lazy val regline:Double=>Double = regressionline(reg._1,reg._2)
        
    lazy val resi:Vector[Double]=residual(time,raw,regline)

    
    //Operation-------------------------------------------
    
    def ts=new tsda(x) 
    //時系列データ化
    
    def ::(component:data)=dase(component,this)
    //２つのdaを１つのdsにする
        
	def summary={ 
	    Vector(name+" : "+raw,"length : "+n,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
}

//コンパニオンオブジェクト
object data{
  def apply(in:Vector[Double])=new data(in)
  //方向を暗黙的引数として入れる必要あり
}
package datafactory
import Converter._
import scala.math._

class data(x:Vector[Double]) extends Descritive{

//Descriptive　※データを標本として捉えている
  
    var name:String="data"		       
    	//mutable   
    
    val n=x.length
    
    val raw=x.toVector
    
    val mean=meanf(x)
    
	val dv=deviation(x,mean)

	val vari=unbiased_variance(devi_squared(dv))
    //不偏分散：標本から行う、母分散の推定値
 
    val sd=sqrt(vari)
    //データを標本と見なし不偏分散を用いて算出した母標準偏差推定値。標本自体の標準偏差が必要ならsamplesdを使うべし
    
    lazy val samplesd=popstdevi(devi_squared(dv))
    //標本データ自体の標準偏差。母集団推定をしない場合に用いる

    lazy val biasvari=biased_variance(devi_squared(dv))
    //標本自体の分散。母分散の推定値ではない。
   
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
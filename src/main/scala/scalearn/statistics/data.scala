package scalearn.statistics

import Converter._
import scala.math._

case class data(raw:Vector[Double]){

//Descriptive　※データを標本として捉えている
  
    var name = "data"		       
    //mutable   
    
    val size = raw.length
    
    val sum = raw.sum

    val mean = sum/size
    
    lazy val dv = Stats.deviation(raw,mean)
	
    lazy val squaredsum = raw.map(pow(_,2)).sum
	
    val dvsquared = raw.map(Stats.each_devi_squared(mean))
	
    val dvsquaredsum = dvsquared.sum

    val vari = Stats.unbiased_variance(dvsquaredsum,size)
    //不偏分散：標本から行う、母分散の推定値

    lazy val samplevari = Stats.biased_variance(dvsquaredsum,size)
    //標本自体の分散。母分散の推定値ではない。

    val sd = sqrt(vari)
    //データを標本と見なし不偏分散を用いて算出した母標準偏差推定値。標本自体の標準偏差が必要ならsamplesdを使うべし
    
    lazy val samplesd = Stats.samplestdevi(dvsquaredsum,size)
    //標本データ自体の標準偏差。母集団推定をしない場合に用いる
    
    
   
    lazy val time:Vector[Double] = (1 to size).map(a=>a.toDouble).toVector
    
    lazy val timemean = time.sum/size
    
    lazy val timedv = Stats.deviation(time,timemean)
    
    lazy val timedvsquared = Stats.devi_squared(timedv)
    
    lazy val timedvsquaredsum = timedvsquared.sum
    
    lazy val timesd = Stats.stdevi(timedvsquaredsum,size)
    
    lazy val timezipdv = timedv.zip(dv)
    
    lazy val timecovari = Stats.covariance(timezipdv)
    
    lazy val timepear = Stats.pearson(timecovari,timesd,sd)
      
    
    lazy val reg:(Double,Double) = Stats.regression(timepear,timesd,sd,timemean,mean)	 //x軸が時間軸のケース。
    
    lazy val regline:Double=>Double = Stats.regressionline(reg._1,reg._2)
        
    def resi:data = data(Stats.residual(time,raw,regline))

    
    //Operation-------------------------------------------
    
    def tots = new tsda(raw)
    //時系列データ化
    
    def ::(component:data) = dase(component,this)
    //２つのdataを１つのdaseにする

    /*
    def draw={
    	val chart:ChartSeries1DMeasure=new ChartSeries1DMeasure(name)
    	x.foreach(elt=>chart.addValue(elt))
    	chart.plot()
    }
    * 
    */
    
    def summary = { 
        Vector(name+" : "+raw,"length : "+size,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	}
}
 

object data{
    
    def apply(raw: Double*): data = data(raw.toVector)
        
}

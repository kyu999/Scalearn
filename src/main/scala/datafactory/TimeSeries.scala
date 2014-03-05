package datafactory

import scala.math._

trait TimeSeries extends Descritive{
	def autocovariance(raw:Vector[Double],lag:Int):Double={
	  lazy val mean=meanf(raw)
	  lazy val lag_raw=raw.drop(lag)
	  if(raw.length<=lag) 0  
	  else lag_raw.zip(raw).map{x=>(x._1-mean)*(x._2-mean)}.reduce{(a,b)=>a+b}/raw.length
	  }
	/*
	 r(h)=E{( Xt - µt )*( Xt+h - µt+h )} 
	 時間軸をずらすだけなのでµt==µt+h。２つの関数がかぶっていない部分は常に自己共分散が0になるので
	 省略してあたかもデータを削ったかのように見えるがそれは実装上の見かけである。lag_rawは時間軸に対して左にずらしている
	t  -1 0 1 2 3 4
	x       6 3 2 1
   x-h  6 3 2 1	(if h is 2. it doesn't matter whether h is positive or negative)
       get only value when t=1~2
	*/
		
	
	def autocorrelation(raw:Vector[Double]):(Double,Vector[Double])={
	  val criteria=2/sqrt(raw.length)
	  val r0=autocovariance(raw,0)
	  if (raw.length<20) (criteria, (0 to raw.length).map(x=>autocovariance(raw,x)/r0).toVector )
	  else (criteria, (0 to 20).map(x=>autocovariance(raw,x)/r0).toVector )
	}
	//acf=r(h)/r(0) , criteriaは有意かどうかのライン。Rの点線のとこ。
	
	def partialhelper(raw:Vector[Double],lag:Int)={

	  if(raw.length<=lag) { 0 }
	  
	  else {
	    
	  val cut_raw=raw.dropRight(lag)
	  val lag_raw=raw.drop(lag)
	  val time=(1 to raw.length).map(a=>a.toDouble).toVector

	  val cutmean=meanf(cut_raw)
	  val lagmean=meanf(lag_raw)
	  val timemean=meanf(time)
	  
	  val cutdevi=deviation(cut_raw,cutmean)
	  val lagdevi=deviation(lag_raw,lagmean)
	  val timedevi=deviation(time,timemean)

	  val zipcut=timedevi.zip(cutdevi)
	  val ziplag=timedevi.zip(lagdevi)
	  
	  val cutsd=stdevi(devito2(cutdevi))
	  val lagsd=stdevi(devito2(lagdevi))
	  val timesd=stdevi(devito2(timedevi))
	  
	  val cutpear=pearson(covariance(zipcut),timesd,cutsd)
	  val lagpear=pearson(covariance(ziplag),timesd,lagsd)
	  
	  val cutreg=regression(cutpear,timesd,cutsd,timemean,cutmean)
	  val lagreg=regression(lagpear,timesd,lagsd,timemean,lagmean)
	  
	  val cutregline=regressionline(cutreg._1,cutreg._2)
	  val lagregline=regressionline(lagreg._1,lagreg._2)
	  
	  //残差ゲット
	  
	  val cutresi=residual(time,cut_raw,cutregline)
	  val lagresi=residual(time,lag_raw,lagregline)
	  
	  val cutresimean=meanf(cutresi)
	  val lagresimean=meanf(lagresi)
	  
	  val cutresidevi=deviation(cutresi,cutresimean)
	  val lagresidevi=deviation(lagresi,lagresimean)
	  val zippedresi=cutresidevi.zip(lagresidevi)
	  
	  val cutresisd=stdevi(devito2(cutresidevi))
	  val lagresisd=stdevi(devito2(lagresidevi))
	  
	  val resultpearson=pearson(covariance(zippedresi),cutresisd,lagresisd)
	  
	  resultpearson
	  
	  	}
	}
	
	def partialacf(raw:Vector[Double])={
	  val criteria=2/sqrt(raw.length)
	  if (raw.length<20) (criteria, (0 to raw.length-1).map(x=>partialhelper(raw,x)).toVector )
	  else (criteria, (0 to 20).map(x=>x).toVector )
	}
	
	//偏自己相関実装予定
	
	def differencing(raw:Vector[Double]):Vector[Double]={
		val zipped=raw.tail.zip(raw.init)
		zipped.map(a=>a._1-a._2)
	}
	/*
	 t	1  2  3  4  5  6 
	Xt  5  8  6  9 10  11
   Xt-1    5  8  6  9  10  11
   ΔXt     3  -2 3  1   1
	 */
}
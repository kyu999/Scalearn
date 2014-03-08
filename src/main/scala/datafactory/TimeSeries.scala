package datafactory

import scala.math._

trait TimeSeries extends Descritive{
	def autocovariance(raw:Seq[Double],lag:Int):Double={
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
		
	
	def autocorrelation(raw:Seq[Double]):(Double,Seq[Double])={
	  val criteria=2/sqrt(raw.length)
	  val r0=autocovariance(raw,0)
	  if (raw.length<20) (criteria, (0 to raw.length).map(x=>autocovariance(raw,x)/r0).toSeq )
	  else (criteria, (0 to 20).map(x=>autocovariance(raw,x)/r0).toSeq )
	}
	//acf=r(h)/r(0) , criteriaは有意かどうかのライン。Rの点線のとこ。
	
	def partialhelper(raw:Seq[Double],lag:Int)={
	  
	  if(raw.length<=lag) { 0 }
	  
	  else {
	    
	  val cut_raw=raw.dropRight(lag)
	  val lag_raw=raw.drop(lag)
	  val time=(1 to cut_raw.length).map(a=>a.toDouble).toSeq
	  	  
	  val cutreg=regRaw(time,cut_raw)
	  val lagreg=regRaw(time,lag_raw)
	  
	  val cutresi=residual(time,cut_raw,regressionline(cutreg))
	  val lagresi=residual(time,lag_raw,regressionline(lagreg))
	  	
	  /*
	  println("-------------------------------")
	  println("lag == "+lag)
	  println("cut_raw"+" is "+cut_raw)
	  println("lag_raw"+" is "+lag_raw)
	  println("time"+time)
	  println("cut_reg"+" is "+cutreg)
	  println("lag_reg"+" is "+lagreg)
	  println("cutresi"+" is "+cutresi)
	  println("lagresi"+" is "+lagresi)
	  println("result is "+	pearRaw(cutresi,lagresi))
	  println("-------------------------------")
	  * 
	  */

	  pearRaw(cutresi,lagresi)
	  
	  	}
	}
	
	def partialacf(raw:Seq[Double])={
	  val criteria=2/sqrt(raw.length)
	  if (raw.length<20) (criteria, (0 to raw.length).map(x=>partialhelper(raw,x)).toSeq )
	  else (criteria, (0 to 20).map(x=>x).toSeq )
	}
	
	//偏自己相関実装予定
	
	def differencing(raw:Seq[Double]):Seq[Double]={
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
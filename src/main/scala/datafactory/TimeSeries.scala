package datafactory

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
		
	
	def autocorrelation(raw:Seq[Double]):IndexedSeq[Double]={
	  val r0=autocovariance(raw,0)
	  if (raw.length<20) (0 to raw.length).map(x=>autocovariance(raw,x)/r0)
	  else (0 to 20).map(x=>autocovariance(raw,x)/r0)
	}
	//acf=r(h)/r(0)
	
//	def detrending(ra:Seq[Double])=regression()
//	def differencing()
}
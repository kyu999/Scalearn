package datastore

trait TimeSeries extends Basic{
	def autocovaiance(raw:Seq[Double],lag:Int):Double={
	  lazy val mean=meanf(raw)
	  lazy val lag_raw=raw.drop(lag)
	  if(raw.length-1<=lag) 0  
	  else lag_raw.zip(raw).map{x=>(x._1-mean)*(x._2-mean)}.reduce{(a,b)=>a+b}/raw.length
	  }
	/*
	 r(h)=E{( Xt - µt )*( Xt+h - µt+h )}
	 時間軸をずらすだけなのでµt==µt+h。２つの関数がかぶっていない部分は常に自己共分散が0になるので
	 省略してあたかもデータを削ったかのように見えるがそれは実装上の見かけである。
	*/
		
	
//	def autocorrelation()
//	def detrending()
//	def differencing()
}
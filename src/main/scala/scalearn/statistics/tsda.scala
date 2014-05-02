package scalearn.statistics

class tsda(raw:Vector[Double]) extends data(raw) with TimeSeries{

	lazy val acov:IndexedSeq[Double] = (0 to raw.length).map(a=>autocovariance(raw,a))
	
	val acf:(Double,Vector[Double]) = autocorrelation(raw,20)
	
	lazy val pacf = partialacf(raw)
	//pacfは未完
	
	
//Operation
	
	def differencing:tsda = new tsda(differencing(raw))
	//differencing後の新たなtsdaインスタンス作成
	
	def detrending:tsda = new tsda(residual(time,raw,regline))
	//detrending後の新たなtsdaインスタンス作成
}
 
object tsda{ 
  def apply(raw:Double*)=new tsda(raw.toVector)
}
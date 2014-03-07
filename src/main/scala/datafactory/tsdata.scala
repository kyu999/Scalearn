package datafactory

class tsdata(raw:Vector[Double]) extends data(raw) with TimeSeries{

	lazy val acov=(0 to raw.length).map(a=>autocovariance(raw,a))
	val acf=autocorrelation(raw)
	lazy val pacf=partialacf(raw)
	val diff=differencing(raw)
	//偏自己相関実装予定
}

object tsdata{
  def apply(raw:Double*)=new tsdata(raw.toVector)
}
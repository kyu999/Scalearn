package datafactory


class tsda(raw:Vector[Double]) extends data(raw) with TimeSeries{

	lazy val acov=(0 to raw.length).map(a=>autocovariance(raw,a))
	val acf=autocorrelation(raw,20)
	lazy val pacf=partialacf(raw)
	val diff=differencing(raw)
	//偏自己相関実装予定
}

object tsda{
  def apply(raw:Double*)=new tsda(raw.toVector)
}
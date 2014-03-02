package datafactory

class tsdata(raw:Vector[Double]) extends data(raw) with TimeSeries{

	val acf=autocorrelation(raw)
	val diff=differencing(raw)
	//偏自己相関実装予定
}

object tsdata{
  def apply(raw:Double*)=new tsdata(raw.toVector)
}
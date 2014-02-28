package datafactory

class tsdata(raw:Seq[Double]) extends data(raw) with TimeSeries{

	val acf=autocorrelation(raw)
	val diff=differencing(raw)

}

object tsdata{
  def apply(raw:Double*)=new tsdata(raw)
}
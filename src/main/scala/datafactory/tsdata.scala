package datafactory

class tsdata(in:Seq[Double]) extends data(in) with TimeSeries{

	lazy val acf=autocorrelation(in)

}

object tsdata{
  def apply(x:Seq[Double])=new tsdata(x)
}
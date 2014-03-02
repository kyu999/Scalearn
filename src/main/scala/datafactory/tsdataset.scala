package datafactory

class tsdataset(tsdatalist:Vector[tsdata]) extends dataset(tsdatalist){
	
	val acf:Vector[(Double,Vector[Double])]=tsdatalist.map(_.acf)
	lazy val diff:Vector[Vector[Double]]=tsdatalist.map(_.diff)
	
}

object tsdataset{
  def apply(x:tsdata*)=new tsdataset(x.toVector)
}
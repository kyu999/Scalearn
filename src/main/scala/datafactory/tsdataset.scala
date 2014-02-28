package datafactory

class tsdataset(tsdatalist:Seq[tsdata]) extends dataset(tsdatalist){
	
	val acf:Seq[IndexedSeq[Double]]=tsdatalist.map(_.acf)
	lazy val diff:Seq[Seq[Double]]=tsdatalist.map(_.diff)
	
}

object tsdataset{
  def apply(x:tsdata*)=new tsdataset(x)
}
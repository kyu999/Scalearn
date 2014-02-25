package datafactory

class data(x:Seq[Double]) extends Descritive with TimeSeries{
  //Descriptive
    var name="data"		//mutable
      
    val raw:Seq[Double]=x 
    val mean=meanf(x)
	val dv=deviation(x,mean)
    val sd=stdevi(devito2(dv))
    
    
  //TimeSeries
    lazy val acf=autocorrelation(x)
  //Operation
    def ::(component:data)=dataset(x,component.raw)
    
	def summary={ 
	    Seq("X; "+raw,"mean -> "+mean,"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
//	summary(x)
}
object data{
  def apply(x:Seq[Double])=new data(x)
}
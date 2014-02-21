package datastore

class data(x:Vector[Double]) extends Basic{
    val raw=x 
	val dv=deviation(x,mean(x))
    val sd=stdevi(devito2(dv))
	def summary(x:Vector[Double])={ 
	    Vector("X; "+raw,"mean -> "+mean(x),"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
	summary(x)
}
object data{
  def apply(x:Vector[Double])=new data(x)
}
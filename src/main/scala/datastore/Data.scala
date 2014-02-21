package datastore

class data(x:Stream[Double]) extends Basic{
    val raw=x 
	val dv=deviation(x,mean(x))
    val sd=stdevi(devito2(dv))
	def summary(x:Stream[Double])={ 
	    Stream("X; "+raw,"mean -> "+mean(x),"deviation -> "+dv,
	        "standard deviation -> "+sd).foreach(println)
	    mkLine
	}
	summary(x)
}
object data{
  def apply(x:Stream[Double])=new data(x)
}
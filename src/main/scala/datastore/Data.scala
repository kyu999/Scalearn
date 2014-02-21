package datastore

class Data(x:Stream[Double]) extends Basic{
	val dev1=dev(x,ave(x))
    val sdX=sd(devto2(dev1))
	def summary(x:Stream[Double])={ 
	    Stream("","X; "+x,"mean -> "+ave(x),"deviation -> "+dev1,
	        "standard deviation -> "+sdX).foreach(println)
	}
	summary(x)
}
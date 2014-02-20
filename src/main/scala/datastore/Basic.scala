package datastore

trait Basic{
	def ave(data:Stream[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def dev(data:Stream[Double],average:Double):Stream[Double]=data.map(x=>x-average)
	//各偏差
	def devto2(devdata:Stream[Double]):Stream[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	def sd(dv2:Stream[Double]):Double=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差
	def summary(x:Stream[Double])={ 
	    	val xav=ave(x)
	    	val dev1=dev(x,xav)
	    val dev2=devto2(dev1)
	    val sdX=sd(dev2)
	    	
	    Stream("","X; "+x,"mean -> "+xav,"deviation -> "+dev1,
	        "deviation^2 -> "+dev2,"standard deviation -> "+sdX).foreach(println)

	}
}
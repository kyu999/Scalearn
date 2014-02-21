package datastore

trait Basic{ 
	def mean(data:Vector[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def deviation(data:Vector[Double],average:Double):Vector[Double]=data.map(x=>x-average)
	//各偏差
	def devito2(devdata:Vector[Double]):Vector[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	def stdevi(dv2:Vector[Double]):Double=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差
	
	def mkLine=println("-------------------------------------------------------------")
}
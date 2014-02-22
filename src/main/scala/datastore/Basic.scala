package datastore

trait Basic{ 
	def meanf(data:Seq[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def deviation(data:Seq[Double],average:Double):Seq[Double]=data.map(x=>x-average)
	//各偏差
	def devito2(devdata:Seq[Double]):Seq[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	def stdevi(dv2:Seq[Double]):Double=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差
	
	def mkLine=println("-------------------------------------------------------------")
}
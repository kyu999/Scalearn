package datastore

//import scala.collection.parallel.immutable.ParSeq
class Dataset(x:Stream[Double],y:Stream[Double]){
//	def read(newvar:Stream[Double])={x=newvar}
	def ave(data:Stream[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def dev(data:Stream[Double],average:Double):Stream[Double]=data.map(x=>x-average)
	//各偏差

	def devto2(devdata:Stream[Double]):Stream[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	def sd(dv2:Stream[Double])=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差
	def zipdev(dev1:Stream[Double],dev2:Stream[Double]):Stream[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	def cov(zipdata:Stream[(Double,Double)]):Double=zipdata.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipdata.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	def pear(covari:Double,variX:Double,variY:Double):Double=covari/(variX*variY)
	//２つのデータセットのピアソン相関関係
	
	val xav=ave(x)
	val yav=ave(y)
	val xdev1=dev(x,xav)
	val ydev1=dev(y,yav)
	val xdev2=devto2(xdev1)
	val ydev2=devto2(ydev1)
	val sdX=sd(xdev2)
	val sdY=sd(ydev2)
	
	val zipXY=zipdev(xdev1,ydev1)
	val covXY=cov(zipXY)
	val pearson=pear(covXY,sdX,sdY)	//  	peason=covariance / (SD of X * SD of Y)
	
	def summary={
		println(" ")
		val xsum=Stream("","X; "+x,"mean -> "+xav,"deviation -> "+xdev1,"deviation^2 -> "+xdev2,"standard deviation -> "+sdX)
		val ysum=Stream("","Y; "+y,"mean -> "+yav,"deviation -> "+ydev1,"deviation^2 -> "+ydev2,"standard deviation -> "+sdY)
		val sumxy=Stream("","covariance -> "+covXY,"peason's correlation -> "+pearson)
		Stream(xsum,ysum,sumxy).foreach(x=>x.foreach(println))
	}
}

package datastore

class Dataset(x:Stream[Double],y:Stream[Double]) extends Basic with Correlation{
	
	val xdev=dev(x,ave(x))
	val ydev=dev(y,ave(y))
	val sdX=sd(devto2(xdev))
	val sdY=sd(devto2(ydev))
	
	val covXY=cov(zipdev(xdev,ydev))
	val pearson=pear(covXY,sdX,sdY)	//  	peason=covariance / (SD of X * SD of Y)
	//peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	
	val spcor=spearman(difsqured(labeling(x),labeling(y)))
	
    def summary={
		println(" ")
		val xsum=Stream("","X; "+x,"mean -> "+ave(x),"deviation -> "+xdev,"standard deviation -> "+sdX)
		val ysum=Stream("","Y; "+y,"mean -> "+ave(y),"deviation -> "+ydev,"standard deviation -> "+sdY)
		val sumxy=Stream("","covariance -> "+covXY,"peason's correlation -> "+pearson,"spearman's correlation -> "+spcor)
		Stream(xsum,ysum,sumxy).foreach(x=>x.foreach(println))
	}
}

package datastore

class Dataset(x:Stream[Double],y:Stream[Double]) extends Basic with Correlation{
	
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
	//peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	
	val labelX=labeling(x)
	val labelY=labeling(y)
	val ranked=rankzip(labelX,labelY)
	val spcor=spearman(difsqured(ranked))
	
    def summary={
		println(" ")
		val xsum=Stream("","X; "+x,"mean -> "+xav,"deviation -> "+xdev1,"deviation^2 -> "+xdev2,"standard deviation -> "+sdX)
		val ysum=Stream("","Y; "+y,"mean -> "+yav,"deviation -> "+ydev1,"deviation^2 -> "+ydev2,"standard deviation -> "+sdY)
		val sumxy=Stream("","covariance -> "+covXY,"peason's correlation -> "+pearson,"spearman's correlation -> "+spcor)
		Stream(xsum,ysum,sumxy).foreach(x=>x.foreach(println))
	}
}

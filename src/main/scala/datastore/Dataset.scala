package datastore

class dataset(rawX:Stream[Double],rawY:Stream[Double]) extends Basic with Correlation{

    val x=data(rawX)
	val y=data(rawY)
	 
	val cov=covariance(zipdevi(x.dv,y.dv))
	val pear=pearson(cov,x.sd,y.sd)	
	val spcor=spearman(difsqured(labeling(x.raw),labeling(y.raw)))
	
    def summary={
		val xsum=Stream("X; "+x.raw,"mean -> "+mean(x.raw),"deviation -> "+x.dv,"standard deviation -> "+x.sd)
		val ysum=Stream("","Y; "+y.raw,"mean -> "+mean(y.raw),"deviation -> "+y.dv,"standard deviation -> "+y.sd)
		val sumxy=Stream("","covariance -> "+cov,"peason's correlation -> "+pear,"spearman's correlation -> "+spcor)
		Stream(xsum,ysum,sumxy).foreach(x=>x.foreach(println))
		mkLine
	}
	summary	//コンストラクタ
}

//コンパニオンオブジェクトを作成。applyでファクトリメソッドを定義しているのでnewが不要になる
object dataset{
  def apply(rawX:Stream[Double],rawY:Stream[Double])=new dataset(rawX,rawY)
}


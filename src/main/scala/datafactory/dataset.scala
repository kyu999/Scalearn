package datafactory

class dataset(rawX:Seq[Double],rawY:Seq[Double]) extends Basic with Correlation{

    val x=data(rawX)			//raw->dataインスタンス作成
	val y=data(rawY)
	 
	lazy val cov=covariance(zipdevi(x.dv,y.dv))
	lazy val pear=pearson(cov,x.sd,y.sd)	
	lazy val spcor=spearman(difsqured(labeling(rawX,rawY)))
   
	//データが多い時のために遅延評価に。
	
    def summary={
		val xsum=Seq("X; "+x.raw,"mean -> "+x.mean,"deviation -> "+x.dv,"standard deviation -> "+x.sd)
		val ysum=Seq("","Y; "+y.raw,"mean -> "+y.mean,"deviation -> "+y.dv,"standard deviation -> "+y.sd)
		val sumxy=Seq("","covariance -> "+cov,"peason's correlation -> "+pear,"spearman's correlation -> "+spcor)
		Seq(xsum,ysum,sumxy).foreach(x=>x.foreach(println))
		mkLine
	}
	//summary	//コンストラクタ
}

//コンパニオンオブジェクトを作成。applyでファクトリメソッドを定義しているのでnewが不要になる
object dataset{
  def apply(rawX:Seq[Double],rawY:Seq[Double])=new dataset(rawX,rawY)
}


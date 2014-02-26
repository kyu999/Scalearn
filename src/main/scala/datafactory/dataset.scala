package datafactory

class dataset(raw:Seq[Seq[Double]]) extends Descritive 
			with Inference with Bayes with TimeSeries with Multivariate{
    
    val datalist:Seq[data]=raw.map(a=>data(a))
    val mean=datalist.map(_.mean)
    val sd=datalist.map(_.sd)
    
    lazy val combi:Seq[Seq[data]]=datalist.combinations(2).toVector
	lazy val covar=combi.map{a=>covariance(zipdevi(a(0).dv,a(1).dv))}.toVector
	lazy val pears=combi.map{a=>
	   pearson(
	       covariance(
	           zipdevi(a(0).dv,a(1).dv) )
	           ,a(0).sd,a(1).sd)
	   }.toVector
	lazy val spears=combi.map{a=>spearman(difsqured(labeling(a(0).raw,a(1).raw)))}
	//データが多い時のために遅延評価に。
	   
	lazy val reg=combi.zip(pears).map{a=>regression(a._2,a._1(0).sd,a._1(1).sd,a._1(0).mean,a._1(1).mean)}
	//regressionの引数は順に、相関係数、XのSD,YのSD、Xの平均、Yの平均
	   
	def naming(in:Seq[Any]*)=in.zip(datalist).foreach{a=>a._2.name=a._1}
    def summary={
        mkLine 
        println("name list : ")
        datalist.foreach(x=>println(x.name))
		mkLine
		println("mean : ")
		mean.foreach(println)
		mkLine
		println("sd : ")
		sd.foreach(println)
		mkLine
		println("combination : ")
		combi.foreach(a=>println(a(0).name+" & "+a(1).name))
		mkLine
		println("covariance : ")
		covar.foreach(println)
		mkLine
		println("pearson's correlation : ")
		pears.foreach(println)
		mkLine
		println("spearman's correlation : ")
		spears.foreach(println)
		mkLine
	}
	
}

//コンパニオンオブジェクトを作成。applyでファクトリメソッドを定義しているのでnewが不要になる
object dataset{
  def apply(raw:Seq[Double]*)=new dataset(raw)
}

//applyメソッドの引数は可変長引数Seq[Double]*にして、class定義の方ではSeq[Seq[Double]]としている。
//こうすることでdata(x,y,z,a,b,c)みたいに書ける。

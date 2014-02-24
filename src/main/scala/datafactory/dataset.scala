package datafactory

class dataset(raw:Seq[Seq[Double]]) extends Descritive 
			with Inference with Bayes with TimeSeries with Multivariate{
    
    
    val datalist:Seq[data]=raw.map(a=>data(a))
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
	
	//Operation
//	def ::(component:data)=dataset(List(component,component))
	
    def summary={
		mkLine
		println("mean : ")
		datalist.foreach(x=>println(x.mean))
		mkLine
		println("sd : ")
		datalist.foreach(x=>println(x.sd))
		mkLine
		println("combination : ")
		combi.foreach(println)
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

package datafactory

trait Descritive{ 
  
	def meanf(data:Seq[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def deviation(data:Seq[Double],average:Double):Seq[Double]=data.map(x=>x-average)
	//各偏差
	def devito2(devdata:Seq[Double]):Seq[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	
	def stdevi(dv2:Seq[Double]):Double=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差
	
	
	def zipdevi(dev1:Seq[Double],dev2:Seq[Double]):Seq[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	def covariance(zipdata:Seq[(Double,Double)]):Double=zipdata.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipdata.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	def pearson(covari:Double,sdX:Double,sdY:Double):Double=covari/(sdX*sdY)
	//２つのデータセットのピアソン相関関係
 	
	
	def labeling(rawX:Seq[Double],rawY:Seq[Double])={
	  val unzipped=rawX.zip(rawY).unzip
	  val placeX=unzipped._1.sorted.zipWithIndex.toMap
	  val placeY=unzipped._2.sorted.zipWithIndex.toMap
	  val rankedX=unzipped._1.map{a=>placeX(a)}
	  val rankedY=unzipped._2.map{a=>placeY(a)}
	  rankedX.zip(rankedY)
	}
	
	
	def difsqured(zippedlabel:Seq[(Int,Int)]):Seq[Double]=zippedlabel.map{x=>math.pow(x._1-x._2,2)}
	//2つのlabelデータをzip化して、対となる値の差をそれぞれ求めて２乗する
	def spearman(difsqudata:Seq[Double])=1-(6*difsqudata.reduce{(a,b)=>a+b})/(math.pow(difsqudata.length,3)-difsqudata.length)
	/*スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:4　にしてる。
  	  peason=covariance / (SD of X * SD of Y)
	  peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	  * 
	  */

	def mkLine=println("-------------------------------------------------------------")
}
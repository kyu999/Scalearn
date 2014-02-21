package datastore

trait Correlation {
	def zipdevi(dev1:Stream[Double],dev2:Stream[Double]):Stream[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	def covariance(zipdata:Stream[(Double,Double)]):Double=zipdata.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipdata.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	def pearson(covari:Double,variX:Double,variY:Double):Double=covari/(variX*variY)
	//２つのデータセットのピアソン相関関係
 	
	
	def labeling(data:Stream[Double])={
	  val placeI=data.sorted.zipWithIndex.toMap
	  data.map(a=>placeI(a))
	}
	def difsqured(label1:Stream[Int],label2:Stream[Int]):Stream[Double]=label1.zip(label2).map(x=>math.pow(x._1-x._2,2))
	//2つのlabelデータをzip化して、対となる値の差をそれぞれ求めて２乗する
	def spearman(difsqudata:Stream[Double])=1-(6*difsqudata.reduce{(a,b)=>a+b})/(math.pow(difsqudata.length,3)-difsqudata.length)
	/*スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:4　にしてる。
  	  peason=covariance / (SD of X * SD of Y)
	  peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	  * 
	  */
	}
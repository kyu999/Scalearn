package datastore

trait Correlation {
	def zipdev(dev1:Stream[Double],dev2:Stream[Double]):Stream[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	def cov(zipdata:Stream[(Double,Double)]):Double=zipdata.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipdata.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	def pear(covari:Double,variX:Double,variY:Double):Double=covari/(variX*variY)
	//２つのデータセットのピアソン相関関係
	
	
	def labeling(data:Stream[Double])={
	  val placeI=data.sorted.zipWithIndex.map(a=>(a._1,a._2+1)).toMap
	  data.map(a=>placeI(a))
	}
	def rankzip(data1:Stream[Int],data2:Stream[Int]):Stream[(Int,Int)]=data1.zip(data2)
	//2つのデータをソートしてzip
	def difsqured(zipped:Stream[(Int,Int)]):Stream[Double]=zipped.map(x=>math.pow(x._1-x._2,2))
	//対となる値の差をそれぞれ求めて２乗する
	def spearman(dsdata:Stream[Double])=1-(6*dsdata.reduce{(a,b)=>a+b})/(math.pow(dsdata.length,3)-dsdata.length)
	//スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:5　にしてる。
}
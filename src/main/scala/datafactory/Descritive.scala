package datafactory

trait Descritive{ 
  
	def meanf(data:Vector[Double]):Double=data.reduce((a,b)=>a+b)/data.length
	//平均
	def deviation(data:Vector[Double],average:Double):Vector[Double]=data.map(x=>x-average)
	//各偏差
	def devito2(devdata:Vector[Double]):Vector[Double]=devdata.map(x=>(math.pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	
	def stdevi(dv2:Vector[Double]):Double=math.sqrt(dv2.reduce((a,b)=>a+b)/(dv2.length-1))	
	//√分散＝標準偏差.ただしこれは不偏分散。通常標本分散は母集団分散よりも小さくなりがちなので標本抽出による偏りを是正するために-1している
	
	
	def zipdevi(dev1:Vector[Double],dev2:Vector[Double]):Vector[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	
	def covariance(zipdata:Vector[(Double,Double)]):Double=zipdata.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipdata.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	
	def pearson(covari:Double,sdX:Double,sdY:Double):Double=covari/(sdX*sdY)
	//２つのデータセットのピアソン相関関係
 	
	
	def labeling(rawX:Vector[Double],rawY:Vector[Double])={
	      if (rawX.length != rawY.length) println("You can't compare different length of variable")
		  val placeX=rawX.sorted.zipWithIndex.toMap
		  val placeY=rawY.sorted.zipWithIndex.toMap
	      val rankedX=rawX.map{a=>placeX(a)}
	      val rankedY=rawY.map{a=>placeY(a)}
	      rankedX.zip(rankedY)	    
	}
	
	
	def difsqured(zippedlabel:Vector[(Int,Int)]):Vector[Double]=zippedlabel.map{x=>math.pow(x._1-x._2,2)}
	//2つのlabelデータをzip化して、対となる値の差をそれぞれ求めて２乗する
	
	def spearman(difsqudata:Vector[Double])=1-(6*difsqudata.reduce{(a,b)=>a+b})/(math.pow(difsqudata.length,3)-difsqudata.length)
	/*スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:4　にしてる。
  	  peason=covariance / (SD of X * SD of Y)
	  peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	  * 
	  */
	
	def regression(corr:Double,xsd:Double,ysd:Double,xmean:Double,ymean:Double):(Double,Double)={
	  val slope=corr*(ysd/xsd)
	  val intercept=ymean-slope*xmean
	  (slope,intercept)
	}
	//この定義の仕方は使い勝手が悪いから考え直す必要あり。
	//回帰直線の傾き；相関係数/(Yの標準偏差＊Xの標準偏差)
	
	def regressionline(slope:Double,intercept:Double):Double=>Double={(x:Double)=>slope*x+intercept}

	def residual(x:Vector[Double],raw:Vector[Double],xregline:(Double=>Double)):Vector[Double]=x.zip(raw).map{a=>
	  a._2-xregline(a._1)
	  }

	def mkLine=println("-------------------------------------------------------------")
}
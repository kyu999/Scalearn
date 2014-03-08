package datafactory
import scala.math._
trait Descritive{ 
  
	def meanf(raw:Seq[Double]):Double=raw.reduce((a,b)=>a+b)/raw.length
	//平均
	def deviation(raw:Seq[Double],average:Double):Seq[Double]=raw.map(x=>x-average)
	//各偏差
	def devito2(devraw:Seq[Double]):Seq[Double]=devraw.map(x=>(pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	
	def stdevi(dvsquared:Seq[Double]):Double=sqrt(dvsquared.reduce((a,b)=>a+b)/(dvsquared.length-1))	
	//√分散＝標準偏差.ただしこれは不偏分散。通常標本分散は母集団分散よりも小さくなりがちなので標本抽出による偏りを是正するために-1している
	
	
	def zipdevi(dev1:Seq[Double],dev2:Seq[Double]):Seq[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	
	def covariance(zipraw:Seq[(Double,Double)]):Double=zipraw.map(x=>x._1*x._2).reduce((a,b)=>a+b)/(zipraw.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	
	def pearson(covari:Double,sdX:Double,sdY:Double):Double=covari/(sdX*sdY)
	//２つのデータセットのピアソン相関関係
	
	def pearRaw(xraw:Seq[Double],yraw:Seq[Double])={
	  
	  if (xraw.length != yraw.length) throw new Exception("can't take dirrerent length of variable")
	  
	  else if(xraw.length<=1) { println("warn : need more than two elements for pearson. X :  "+xraw+" , Y : "+yraw) ; 0.0 }
	  
	  else {
	    
	    val xmean=meanf(xraw)
	    	val ymean=meanf(yraw)
	  
	    	val xdevi=deviation(xraw,xmean)
	    	val ydevi=deviation(yraw,ymean)
	  
	    	val xdevi2=devito2(xdevi)
	    	val ydevi2=devito2(ydevi)
	  
	    	val xsd=stdevi(xdevi2)
	    	val ysd=stdevi(ydevi2)
	  
	    	val zipdevi=xdevi.zip(ydevi)
	  
	  	val covar=covariance(zipdevi)
	  
	  	pearson(covar,xsd,ysd)
	  }
	  
	}
	 	
	
	def labeling(rawX:Seq[Double],rawY:Seq[Double])={
	      if (rawX.length != rawY.length) println("You can't compare different length of variable")
		  val placeX=rawX.sorted.zipWithIndex.toMap
		  val placeY=rawY.sorted.zipWithIndex.toMap
	      val rankedX=rawX.map{a=>placeX(a)}
	      val rankedY=rawY.map{a=>placeY(a)}
	      rankedX.zip(rankedY)	    
	}
	
	
	def difsqured(zippedlabel:Seq[(Int,Int)]):Seq[Double]=zippedlabel.map{x=>pow(x._1-x._2,2)}
	//2つのlabelデータをzip化して、対となる値の差をそれぞれ求めて２乗する
	
	def spearman(difsquared:Seq[Double])=1-(6*difsquared.reduce{(a,b)=>a+b})/(pow(difsquared.length,3)-difsquared.length)
	/*スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:4　にしてる。
  	  peason=covariance / (SD of X * SD of Y)
	  peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	  * 
	  */
	
	def euclidean(xraw:Seq[Double],yraw:Seq[Double])={
	  val sum=xraw.zip(yraw).map(a=>pow(a._1-a._2,2)).reduce((a,b)=>a+b)
	  1/(1+sqrt(sum))	//0で除算してエラーになるのを防ぐため。
	}
	
	def regression(corr:Double,xsd:Double,ysd:Double,xmean:Double,ymean:Double):(Double,Double)={
	  val slope=corr*(ysd/xsd)
	  val intercept=ymean-slope*xmean
	  (slope,intercept)
	}
	//回帰直線の傾き；相関係数/(Yの標準偏差＊Xの標準偏差)
	
	def regRaw(xraw:Seq[Double],yraw:Seq[Double])={
	  
	  if(xraw.length !=yraw.length) throw new Exception("can't take dirrerent length of variable")
	  
	  else if(xraw.length<=1) { println("warn : need more than two elements for regression. X :  "+xraw+" , Y : "+yraw) ; (0.0,0.0) }
	  
	  else {
	  
		val xmean=meanf(xraw)
	  
		val ymean=meanf(yraw)
	  
		val xdevi=deviation(xraw,xmean)
		val ydevi=deviation(yraw,ymean)
	  
		val xdevi2=devito2(xdevi)
	  	val ydevi2=devito2(ydevi)
	  
	  	val xsd=stdevi(xdevi2)
	  	val ysd=stdevi(ydevi2)
	  
	  	val zipdevi=xdevi.zip(ydevi)
	  
	  	val covar=covariance(zipdevi)
	  
	  	val pear=pearson(covar,xsd,ysd)
	  
	  	regression(pear,xsd,ysd,xmean,ymean)
	  
	  }
	  
	}
	
	def regressionline(slope_intercept:(Double,Double)):Double=>Double={(x:Double)=>slope_intercept._1*x+slope_intercept._2}

	def residual(x:Seq[Double],raw:Seq[Double],xregline:(Double=>Double)):Seq[Double]=
	  x.zip(raw).map{ a => a._2-xregline(a._1) }

	def mkLine=println("-------------------------------------------------------------")
}
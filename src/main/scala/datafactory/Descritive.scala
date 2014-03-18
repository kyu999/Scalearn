package datafactory

import scala.math._
import scala.collection.mutable.ListBuffer

trait Descritive{ 
    
	def meanf(raw:Vector[Double]):Double=raw.sum/raw.length
	//平均
	
	def deviation(raw:Vector[Double],average:Double):Vector[Double]=raw.map(each_devi(average))
	//各偏差
		
	def each_devi(average:Double):Double=>Double = {
	  		(elt:Double) => elt - average
	}
	  		
	def each_squared(each_devi:Double=>Double):Double=>Double = {
	  		(elt:Double) => pow(each_devi(elt) ,2) 
	}
	  		
	def each_devi_squared:Double=>(Double=>Double) 
			= each_devi _  andThen each_squared _  		
	//最初のDoubleはaverage、２番目のDoubleはElement.averageを受け取って、elementを受け取ってDoubleを返す関数を返す
	//関数合成をする時は常に前の関数の戻り値を後の関数が受け取れるようにしなければならない	
	
	def devi_squared(devraw:Vector[Double]):Vector[Double]=devraw.map(x=>(pow(x,2))) 
	//各偏差の２乗
	//偏差の２乗して平方根とるんじゃなくて偏差の絶対値をとったほうが効率良いかな？
	
	def biased_variance(dvsquaredsum:Double,n:Int):Double=dvsquaredsum/(n)
	//分散
	
	def unbiased_variance(dvsquaredsum:Double,n:Int):Double=dvsquaredsum/(n-1)
	//不偏分散
	
	def stdevi(dvsquaredsum:Double,n:Int):Double=sqrt(dvsquaredsum/(n-1))	
	//√分散＝標準偏差.ただし不偏分散を使用したため標本標準偏差。通常標本分散は母集団分散よりも小さくなりがちなので標本抽出による偏りを是正するために-1している
	
	def samplestdevi(dvsquaredsum:Double,n:Int):Double=sqrt(dvsquaredsum/n)	
	
	def sterror(unbiase_vari:Double,n:Int)=sqrt(unbiase_vari/n)
	
	def zipdevi(dev1:Vector[Double],dev2:Vector[Double]):Vector[(Double,Double)]=dev1.zip(dev2)
	//2つのデータセットの偏差をTuple化
	
	def covariance(zipraw:Vector[(Double,Double)]):Double=zipraw.map(x=>x._1*x._2).sum/(zipraw.length-1) 
	//2つのデータセット共分散(=対応するXとYの偏差の積の平均）
	
	def pearson(covari:Double,sdX:Double,sdY:Double):Double=covari/(sdX*sdY)
	//２つのデータセットのピアソン相関関係
	
	def pearRaw(xraw:Vector[Double],yraw:Vector[Double])={
	  
	  if (xraw.length != yraw.length) throw new Exception("can't take dirrerent length of variable")
	  
	  else if(xraw.length<=1) { println("warn : need more than two elements for pearson. X :  "+xraw+" , Y : "+yraw) ; 0.0 }
	  
	  else {
	    
	    val xlength=xraw.length
	    val ylength=yraw.length
	    
	    val xmean=meanf(xraw)
	    	val ymean=meanf(yraw)
	  
	    	val xdevi=deviation(xraw,xmean)
	    	val ydevi=deviation(yraw,ymean)
	    	
	    	val xdevi2sum=devi_squared(xdevi).sum
	    	val ydevi2sum=devi_squared(ydevi).sum
	    	
	    	val xsd=stdevi(xdevi2sum,xlength)
	    	val ysd=stdevi(ydevi2sum,ylength)
	  
	    	val zipdevi=xdevi.zip(ydevi)
	  
	  	val covar=covariance(zipdevi)
	  
	  	pearson(covar,xsd,ysd)
	  }
	  
	}
	 	
	
	def labeling(rawX:Vector[Double],rawY:Vector[Double])={
	      if (rawX.length != rawY.length) println("You can't compare different length of variable")
		  val placeX=rawX.sorted.zipWithIndex.toMap
		  val placeY=rawY.sorted.zipWithIndex.toMap
	      val rankedX=rawX.map{a=>placeX(a)}
	      val rankedY=rawY.map{a=>placeY(a)}
	      rankedX.zip(rankedY)	    
	}
	
	
	def difsqured(zippedlabel:Vector[(Int,Int)]):Vector[Double]=zippedlabel.map{x=>pow(x._1-x._2,2)}
	//2つのlabelデータをzip化して、対となる値の差をそれぞれ求めて２乗する
	
	def spearman(difsquared:Vector[Double])=1-(6*difsquared.reduce{(a,b)=>a+b})/(pow(difsquared.length,3)-difsquared.length)
	/*スピアマンの順位相関係数。同じ順位の場合は昇順にしているため若干本来と違う。a:4.5,b:4.5 => a:4,b:4　にしてる。
  	  peason=covariance / (SD of X * SD of Y)
	  peasonは外れ値の影響を受けやすいのと、厳密には正規分布のデータが対象のパラメトリックな手法
	  * 
	  */
	
	def euclidean(xraw:Vector[Double],yraw:Vector[Double])={
	  val sum=xraw.zip(yraw).map(a=>pow(a._1-a._2,2)).sum
	  1/(1+sqrt(sum))	//0で除算してエラーになるのを防ぐため。
	}
	
	def regression(corr:Double,xsd:Double,ysd:Double,xmean:Double,ymean:Double):(Double,Double)={
	  val slope=corr*(ysd/xsd)
	  val intercept=ymean-slope*xmean
	  (slope,intercept)
	}
	//回帰直線の傾き；相関係数/(Yの標準偏差＊Xの標準偏差)
	
	def regRaw(xraw:Vector[Double],yraw:Vector[Double])={
	  
	  if(xraw.length !=yraw.length) throw new Exception("can't take dirrerent length of variable")
	  
	  else if(xraw.length<=1) { println("warn : need more than two elements for regression. X :  "+xraw+" , Y : "+yraw) ; (0.0,0.0) }
	  
	  else {
	  
	    val sizeX=xraw.length
	    val sizeY=yraw.length
	    
		val xmean=meanf(xraw)
	    val ymean=meanf(yraw)
	  
		val xdevi=deviation(xraw,xmean)
		val ydevi=deviation(yraw,ymean)
		
		val xdevi2sum=xraw.map(each_devi_squared(xmean)).sum
	    	val ydevi2sum=yraw.map(each_devi_squared(ymean)).sum
	    	
	    	val xsd=stdevi(xdevi2sum,sizeX)
	    	val ysd=stdevi(ydevi2sum,sizeY)
	  
	  
	  	val zipdevi=xdevi.zip(ydevi)
	  
	  	val covar=covariance(zipdevi)
	  
	  	val pear=pearson(covar,xsd,ysd)
	  
	  	regression(pear,xsd,ysd,xmean,ymean)
	  
	  }
	  
	}
	
	def regressionline(slope_intercept:(Double,Double)):Double=>Double={(x:Double)=>slope_intercept._1*x+slope_intercept._2}

	
	def residual(x:Vector[Double],raw:Vector[Double],xregline:(Double=>Double)):Vector[Double]=
	  x.zip(raw).map{ a => a._2-xregline(a._1) }

	def subtotaling(raw:Vector[Vector[Double]])={
	  	  
	  var stock:ListBuffer[Double]=ListBuffer()
	  
	  var horizontal=true
	  
	  val width=raw.head.length
	  val height=raw.length
	  
	  var lookraw=0
	  var lookcol=0
	  
	  while(horizontal){		
	    
	    var vertical=true
	    var sum=0.0
	  
	    while(vertical){

	      val look=raw(lookcol)(lookraw)
	      
	      sum=sum+look		//縦の繰り返し。Doubleを返す

	      lookcol=lookcol+1

	      if(lookcol>=height) vertical=false
	    
	    }
	    
	    stock+=sum
	    
	    lookcol=0
	    sum=0
	    	    	    
	    if(lookraw>=width-1) horizontal=false

	    lookraw=lookraw+1

	  }
	  
	  stock
	  
	}
	
	def mkLine=println("-------------------------------------------------------------")
}
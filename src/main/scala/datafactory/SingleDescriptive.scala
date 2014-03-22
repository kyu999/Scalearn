package datafactory

import scala.math._
import scala.collection.mutable.ListBuffer

trait SingleDescriptive {

	val raw:Vector[Double]
	
  	def mean:Double=raw.sum/raw.length
	//平均
	
	lazy val dv:Vector[Double]=raw.map(each_devi)
	//各偏差
		
	def each_devi:Double=>Double = {
	  		(elt:Double) => elt - mean
	}
	  		
	def each_squared:Double=>Double = {
	  		(elt:Double) => pow(elt ,2) 
	}
	  		
	val each_devi_squared
			= each_devi andThen each_squared	
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
	

	
	def regression(corr:Double,xsd:Double,ysd:Double,xmean:Double,ymean:Double):(Double,Double)={
	  val slope=corr*(ysd/xsd)
	  val intercept=ymean-slope*xmean
	  (slope,intercept)
	}
	//回帰直線の傾き；相関係数/(Yの標準偏差＊Xの標準偏差)
	
	
	def regressionline(slope_intercept:(Double,Double)):Double=>Double={(x:Double)=>slope_intercept._1*x+slope_intercept._2}

	
	def residual(x:Vector[Double],raw:Vector[Double],xregline:(Double=>Double)):Vector[Double]=
	  x.zip(raw).map{ a => a._2-xregline(a._1) }

	
	def mkLine=println("-------------------------------------------------------------")
}
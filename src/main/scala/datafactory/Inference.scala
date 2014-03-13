package datafactory
import scala.math._
import distribution.t

trait Inference extends Descritive{

	def paired_t_test(rawX:Vector[Double],rawY:Vector[Double]):Boolean={
	 
      val gap=rawX.zip(rawY).map(a=>a._1-a._2)
	  val mean=meanf(gap)
	  val n=gap.length
	  val devisquared=devi_squared(deviation(gap,mean))
	  val unbiased_vari=unbiased_variance(devisquared)
	  val se=sterror(unbiased_vari,n)
	  val tval=mean/se
	  	  
	  t.table(n-1,tval)
	}
  	  //関連2群の検定：帰無仮説=>２つの群は同じ母集団を持つ
	  //false => ２つの群には有為な差が存在する
	  //ｔ値とは、２つの山がどのくらいずれているかを示す値です。たくさんずれていれば「両者は違う山から出てきた標本らしい」という結論が導かれます。
	  //t分布の確率密度関数からp値を取得。。。ただし自由度が絡んでいるので分布表をMapや関数化した方がてっとりばやいかも

	
	def welch_t_test(rawX:Vector[Double],rawY:Vector[Double])={
	  
	  val meanX=meanf(rawX)
	  val meanY=meanf(rawY)	  
	  	  	  
	  val devi2X=devi_squared(deviation(rawX,meanX))
	  val devi2Y=devi_squared(deviation(rawY,meanY))
	  
	  val unbiase_variX=unbiased_variance(devi2X)
	  val unbiase_variY=unbiased_variance(devi2Y)
	  
	  println("variX : "+unbiase_variX)
	  println("variY : "+unbiase_variY)
	  
	  val sizeX=rawX.length
	  val sizeY=rawY.length
	  
	  val vari_averX=unbiase_variX/sizeX
	  val vari_averY=unbiase_variY/sizeY
	  
	  val tval=(meanX-meanY)/sqrt( vari_averX + vari_averX )
	  	  
	  val nume= pow( vari_averX + vari_averY ,2)
	  val denoleft= ( pow(unbiase_variX,2) / ( pow(sizeX,2)*(sizeX-1) ) )
	  val denoright= ( pow(unbiase_variY,2) / ( pow(sizeY,2)*(sizeY-1) ) )
	  
	  val df=nume/(denoleft+denoright)
	  
//	  println("nume : "+nume)
//	  println("denoleft : "+denoleft)
//	  println("denoright : "+denoright)
	  
	  t.table(df.toInt,tval)
	  
	}
	// 等分散検定=>独立2群 の検定は多重検定にあたるためウェルチのみを用いるべきと言う考えにのっとりStudent T検定は実装しない
	
}
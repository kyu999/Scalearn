package datafactory
import scala.math._
trait Inference extends Descritive{
	def paired_t_test(rawX:Seq[Double],rawY:Seq[Double]){
	  val gap=rawX.zip(rawY).map(a=>a._1-a._2)
	  val tval=meanf(gap)/(stdevi(gap)/sqrt(gap.length))
	  //関連2群の検定
	  //ｔ値とは、２つの山がどのくらいずれているかを示す値です。たくさんずれていれば「両者は違う山から出てきた標本らしい」という結論が導かれます。
	  //t分布の確率密度関数からp値を取得。。。ただし自由度が絡んでいるので分布表をMapや関数化した方がてっとりばやいかも
	}
	def student_t_test(rawX:Seq[Double],rawY:Seq[Double])={
	  val m=rawX.length
	  val n=rawY.length
	  val infer_vari=(m-1)*stdevi(rawX)+(n)*stdevi(rawY)
	  val tval=abs(meanf(rawX)-meanf(rawY))/sqrt(infer_vari*(1/m+1/n))
	  
	}
	
	def welch_t_test()
	
	//独立2群の検定
//f examine
}
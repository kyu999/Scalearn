package datafactory
import scala.math._
trait Inference extends Descritive{
	def paired_t_test(rawX:Seq[Double],rawY:Seq[Double]){
	  val gap=rawX.zip(rawY).map(a=>a._1-a._2)
	  val t=meanf(gap)/(stdevi(gap)/sqrt(gap.length))
	  //t分布の確率密度関数からp値を取得。。。ただし自由度が絡んでいるので分布表をMapや関数化した方がてっとりばやいかも
	}
//f examine
}
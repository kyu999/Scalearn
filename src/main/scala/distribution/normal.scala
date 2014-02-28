package distribution
import scala.math._
class normal(mu:Double,sigma:Double) {
	def pdf:Double=>Double={
	val f={
	  (x:Double)=>
	val logged=log(1/sqrt(2*Pi*pow(sigma,2))) - ( pow(x-mu,2) )/(2*pow(sigma,2))
	println("logged : "+logged)
	val ans=pow(E,logged)
	println(ans)
	ans
	}
	f
	}
	//確率密度関数
}
object testes extends App{
  val n=new normal(100,5)
  println(n.pdf(102))
  println(n.pdf(96))
}

//桁数漏れが起こる。=>対数とる？
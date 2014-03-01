package distribution
import scala.math._
object gamma {
	def pdf(n:Int):Double={
	  val logged=n*(log(n)-1)+log(2*Pi*n)/2
	  println("gammma pdf logged : "+logged)
	  logged
	}
	def cul(n:Int):Double={
	  val logged=n*(log(n)-1)+(log(2*Pi*n))/2
	  println("gamma value logged : "+logged)
	  pow(E,logged)
	}
	def factorial(n:Double):Double={
	  if(n<=1) log(1)
	  else log(n)+factorial(n-1)
	}
	//桁数漏れ対策でlogで計算
	
	//cul関数は正しくない。ログの値が４つほどずれている。＝３桁前後ずれている
	/*     
	ガンマ関数の計算にはlengthが小さいなら（自由度が小さいなら）通常のfactorial関数で。
    nが大きく計算にかかる負荷が大きくなりそうならスターリングの近似式を活用。
    log(n!)=>n(log(n)-1)+(1/2)*log(2*Pi*n)
    n!=>√(2*Pi*n)*pow(n,n)*pow(E,-n)
	 
	 powでlogから直す段階で桁数溢れが勃発。logのままで行った方が良いかも
	 */
}

object gammates extends App{
  println(gamma.cul(50))
  (4 to 7).map(a=>gamma.factorial(a)).foreach(println)
  println(gamma.factorial(1000))
}
package distribution
import scala.math._
object gamma {
	
	def stirling(n:Int):Double={
	  val logged=n*(log(n)-1)+(log(2*Pi*n))/2
	  logged
	}
	//n>100くらいから近似でstirlingが使えるかな。n<100まではfactorialで。
	
	def factorial(n:Double):Double={
	  if(n<=1) log(1)
	  else log(n)+factorial(n-1)
	}
	//桁数漏れ対策でlogで計算.
	
	/*     
	ガンマ関数の計算にはlengthが小さいなら（自由度が小さいなら）通常のfactorial関数で。
    nが大きく計算にかかる負荷が大きくなりそうならスターリングの近似式を活用。
    log(n!)=>n(log(n)-1)+(1/2)*log(2*Pi*n)
	 
	 powでlogから直す段階で桁数溢れが勃発。logのままで行った方が良いかも
	 */
}

object gammates extends App{
  println(gamma.stirling(50))
  println("factorial")
  (0 to 10).map(a=>pow(E,gamma.factorial(a))).foreach(println)
  println("stirling")
  (0 to 10).map(a=>pow(E,gamma.stirling(a))).foreach(println)
  println("correct : "+pow(E,363.7)+" , stirling : "+pow(E,gamma.stirling(100))+" , factorial : "+pow(E,gamma.factorial(100)))
}
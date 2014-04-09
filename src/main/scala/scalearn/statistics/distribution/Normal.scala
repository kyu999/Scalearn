package scalearn.statistics.distribution
import scala.math._

class Normal(mu:Double,sigma:Double) {
	def pdf:Double=>Double={
	val f={
	  (x:Double)=>
	val logged=log(1/sqrt(2*Pi*pow(sigma,2))) - ( pow(x-mu,2) )/(2*pow(sigma,2))
//	println("logged : "+logged)
	val ans=pow(E,logged)
	println(ans)
	ans
	}
	f
	}
	//確率密度関数
}
object testes extends App{
  val n=new Normal(100,5)
  println(n.pdf(102))
  println(n.pdf(96))
  
  var nval=n.pdf(100)
  var rate=0.01
  var ok=true
  var sum=0.0
  var counter=0
  while(ok){
    val p=n.pdf(nval)
    if(p<=0) ok=false
    sum=sum+p
    nval=rate-nval
    counter=counter+1
  }
  println("sum: "+sum)
  println("counter : "+counter)
}

//桁数漏れが起こる。=>対数とる？
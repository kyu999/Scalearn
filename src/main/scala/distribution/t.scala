package distribution
import scala.math._

object T {
  
	//α == 0.05, 両側検定
	def table(df:Int,t:Double)={
	  var criteria=0.0
	  val store=Map(1->12.706,2->4.303,3->3.182,4->2.776,5->2.571,6->2.447,7->2.365,8->2.306,9->2.262,10->2.226,11->2.201,12->2.179,13->2.160,14->2.145,15->2.131,16->2.120,17->2.110,18->2.101,19->2.093,20->2.086,21->2.080,22->2.074,23->2.069,24->2.064,25->2.060,26->2.056,27->2.052,28->2.048,29->2.045,30->2.042)
	  store.get(df) match{
	    case Some(x)=>criteria=x
	    case None=>
	      if(df<=40) criteria=2.021
	      else if(df<=60) criteria=2.000
	      else if(df<=120) criteria=1.980
	      else criteria=1.960
	  }
	  
//	  println("df : "+df)
//	  println("criteria : "+criteria)
//	  println("t-value : "+t)
//	  println("帰無仮説：２つは同じ母集団を持つ。２つの標本平均の検定によると。")
	  
	  if(criteria<abs(t)) false
	  else true
	  //false stands for they have significant difference
	  //true means they are from same population
	}

	
	/*
   Steps
    1. 自由度を受け取り確率密度関数をreturnする関数定義
    2. x（ここではt値）を受け取りその確率（p値）を受け取る関数を定義
    3. 実際にt値を入れてp値を得る
    
    ※もしくはガンマ関数（factorial)の確率密度関数を定義した後に公式に当てはめるのもあり。
    ガンマ関数の計算にはlengthが小さいなら（自由度が小さいなら）通常のfactorial関数で。
    nが大きく計算にかかる負荷が大きくなりそうならスターリングの近似式を活用。
    log(n!)=>n(log(n)-1)+(1/2)*log(2*Pi*n)
    n!=>√(2*Pi*n)*pow(n,n)*pow(E,-n)
   */
  def pdf(n:Int,t:Double)={
	
    val logged=log(Gamma.factstir(n/2))-log(Gamma.factstir((n-1)/2))-2*n*log(1+(pow(t,2)/(n-1)))
 //   println("t logged pdf"+logged)
    val ans=pow(E,logged)
    println("ans : "+ans)
    ans
  }
  
}
//正確性に著しく書けるのでどこか間違ってる可能性大。自然科学の統計学で勉強しよう。
object Ttes extends App{
  T.pdf(49,2.0)
  T.pdf(300, 1.967903)
  var tval=1.967903
  var rate=tval*0.001
  var ok=true
  var sum=0.0
  var counter=0
  while(ok){
    val p=T.pdf(100,tval)
    if(p<=0) ok=false
    sum=sum+p
    tval=rate+tval
    counter=counter+1
  }
  println("sum: "+sum)
  println("counter : "+counter)
  
  println(T.table(7,-2.366))
  
  //n=300ではうまく行ったのに対してn=50では明らかにおかしな値だったからfactorial関連でミスってる可能性大。
  //factorialが分数には対処できないのでそこんとこかも。
}

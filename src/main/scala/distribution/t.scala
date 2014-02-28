package distribution
import scala.math._
object t {
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
    val logged=log(gamma.pdf(n/2))-log(gamma.pdf((n-1)/2))-2*n*log(1+(pow(t,2)/(n-1)))
    println("t logged pdf"+logged)
    val ans=pow(E,logged)
    println("t ans pdf "+ans)
  }
  
}
//正確性に著しく書けるのでどこか間違ってる可能性大。
object ttes extends App{
  t.pdf(120, 1.98)
}

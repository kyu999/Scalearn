package datafactory
import Converter._
class matrix(raw:Vector[Vector[Double]]) {
  //matrixのサブクラスに列ベクトル、行ベクトルで構成されたそれぞれのmatrixがある
  //Vector[Vector[Double]]で実質実装。コンストラクタで各Vectorの要素数が同じかどうか確認する必要あり
  /*
   Vector{
   	  Vector(1,2,3,4)
   	  Vector(5,4,3,5)
   	  Vector(5,4,7,6)
   		 }
   
  縦ベクトルはどうするか？dataに方向性をつけベクトル化しよう！
  data=>ベクトル、matrix=>Vectorの集まり
  data*matrix　みたいな感じで計算出来るように。
  てかそもそも基本列ベクトルじゃね？
  */
	val width=raw(0).length
	val height=raw.length
	def matcul(x:Vector[Vector[Double]],y:Vector[Vector[Double]])={
	x.map{a=>println("a: "+a);y.map{b=>println("b : "+b+" ,helper(a,b) : "+helper(a,b));helper(a,b)}}
	}
	def helper(left:Seq[Double],right:Seq[Double])={
	  left.zip(right).map(a=>a._1*a._2).reduce((a,b)=>a+b)
	}
	
}
/*
object matrix{
  def apply(raw:Vector[Vector[Double]])={
    var rule=true
    val width=raw(0).length
    raw.foreach{a=>if(a.length !=width) rule=false}
    if(rule) new matrix(raw)
    else None
  }
  //もしも縦横が整っていないデータ群が来たらNoneしちゃうよ。
}
*/
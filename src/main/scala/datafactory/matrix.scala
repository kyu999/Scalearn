package datafactory
import Converter._
class matrix(raw:Vector[Vector[Double]]) {
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
}

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
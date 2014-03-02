package datafactory
import Converter._
class matrix(raw:Vector[Vector[Double]]){
  //matrixのサブクラスに列ベクトル、行ベクトルで構成されたそれぞれのmatrixがある
  //Vector[Vector[Double]]で実質実装。コンストラクタで各Vectorの要素数が同じかどうか確認する必要あり
  /*
raw=Vector{
   	  Vector(1,2,3,4)
   	  Vector(5,4,3,5)
   	  Vector(5,4,7,6)
   		 }
temp=Vector{
   	  Vector(1,5,5)
   	  Vector(2,4,4)
   	  Vector(3,3,7)
   	  Vector(4,5,6)
   	  }　	となる。
  */
	val width=raw(0).length
	val height=raw.length
    val temp:Vector[Vector[Double]]=(0 to width-1).map{a=>raw.map(b=>b(a))}.toVector
        
	def matcul(x:Vector[Vector[Double]],y:Vector[Vector[Double]])={
	x.map{a=>println("a: "+a);y.map{b=>println("b : "+b+" ,helper(a,b) : "+helper(a,b));helper(a,b)}}
	}
    //Vectorじゃなきゃ遅すぎて駄目。
	//縦ベクトルで構成のmatrix*横ベクトルで構成のmatrix同士じゃないと積が出来ない。
	def helper(left:Vector[Double],right:Vector[Double])={
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
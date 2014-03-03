package math

import datafactory._

class m(raw:Vector[Vector[Double]]) extends Matrix{

//Entity
    val x=raw
	val width=raw(0).length
	val height=raw.length
    val helperT:Vector[Vector[Double]]=(0 to width-1).map{a=>raw.map(b=>b(a))}.toVector

//Operation
    
    def *(component:m)=multiply(raw,component.helperT)
    //可視化のためにこう書いているが本来はnew m(multiply(raw,component.helperT))のように新たなmatrixを返すようにする
    def T=new m(helperT)
    
}

object m{
  def apply(raw:Vector[Double]*)=new m(raw.toVector)
}


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
        
	//println("a: "+a);
	//println("b : "+b+" ,helper(a,b) : "+helper(a,b));
    //Vectorじゃなきゃ遅すぎて駄目。
	//縦ベクトルで構成のmatrix*横ベクトルで構成のmatrix同士じゃないと積が出来ない。

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
package scalearn.math

import scalearn.statistics._

class m(in:Vector[Vector[Double]]) extends Matrix{

    val width=in(0).length
	//コンストラクタ
    in.foreach(a=> if(a.length != width) throw new Exception("not arranged") )
    
//Entity
    val raw = in
    val height = in.length
    val helperT:Vector[Vector[Double]] = (0 to width-1).map{a=>in.map(b=>b(a))}.toVector

//Operation
    def +(component:m) =
      	if( (width != component.width) || (height != component.height) ) 
            throw new Exception("not formatted") 
        else new m(add(in,component.raw))
    
    def -(component:m) =
        if( (width != component.width) || (height != component.height) ) 
            throw new Exception("not formatted") 
        else new m(subtract(in,component.raw))
    
    def *(component:m) =
      if( width != component.height) throw new Exception("not formatted")
      else new m(multiply(in,component.helperT))
    //行列の積
    
    def **(component:Int) = new m(raw.map(a=>a.map(b=>b*component)))
    //スカラー倍
    
    def t = new m(helperT)
    //transpose
}

object m{
    def apply(in:Vector[Double]*) = new m(in.toVector)
}

package scalearn.math

import scalearn.statistics._

case class m(raw:Vector[Vector[Double]]) extends Matrix{

    val width = raw(0).length
	//コンストラクタ
        
    raw.foreach(a=> if(a.length != width) 
               throw new Exception("not arranged") )
    
        
    val height = raw.length
        
    val helperT: Vector[Vector[Double]] = 
        (0 to width-1)
          .map{ a=> raw.map( b => b(a)) }
          .toVector
              
    val checkScale = 
     { component: m => 
        (width != component.width) || (height != component.height) }

//Operation
    def +(component: m) =
        
      	if(checkScale(component)) throw new Exception("not formatted") 
        
        else new m(add(raw,component.raw))
    
    def -(component: m) =
        
        if(checkScale(component)) throw new Exception("not formatted")
        
        else new m(subtract(raw,component.raw))
    
    def *(component: m) =
        
      if( width != component.height) throw new Exception("not formatted")
        
      else new m(multiply(raw,component.helperT))
    //行列の積
    
    def **(component:Int) = new m(raw.map(a=>a.map(b=>b*component)))
    //スカラー倍
    
    def t = new m(helperT)
    //transpose
}

object m{
    def apply(raw: Vector[Double]*) = new m(raw.toVector)
}

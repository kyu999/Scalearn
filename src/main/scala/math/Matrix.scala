package math
import scala.collection.mutable.ArrayBuffer
trait Matrix {
  
	def add(left:Vector[Vector[Double]],right:Vector[Vector[Double]])={
	  def helper(x:Vector[Double],y:Vector[Double])={
	    x.zip(y).map(c=>c._1+c._2)
	  }
	  left.zip(right).map(a=>helper(a._1,a._2))
	}
	
	def subtract(left:Vector[Vector[Double]],right:Vector[Vector[Double]])={
	  def helper(x:Vector[Double],y:Vector[Double])={
	    x.zip(y).map(c=>c._1-c._2)
	  }
	  left.zip(right).map(a=>helper(a._1,a._2))
	}
	
	def multiply(x:Vector[Vector[Double]],y:Vector[Vector[Double]])={
				
		def helper(left:Vector[Double],right:Vector[Double])=
				left.zip(right).map(a=>a._1*a._2).reduce((a,b)=>a+b)
		
	    x.map{a=>y.map{b=>helper(a,b)}}
	}
	
	
	def inverse(in:ArrayBuffer[ArrayBuffer[Double]])={
	  var entity:ArrayBuffer[ArrayBuffer[Double]]=in
	  val width=entity(0).length-1
	  val height=entity.length-1
	  //rowは縦の番号、colは横の番号
	  var col=0
	  var row=0
	 
	  //while 始まる
	  var terminate=false
	  
	  while(!terminate){
	    
	    if(col>=width) terminate = !terminate	//次のループチェックで関数自体終了
	    if(row > height) row=0; col=col+1 //もしありもしない段を見ていたらrowをリセットし１つ横の行へ。処理は続ける
	     
	    //実際の処理
	    
	    var check=true
	    var lookcol=col
	    	val subval=entity(row)(row)

	    //col==rowのケースは最初にやらねばならぬ	  	    	  
	    while(check){
	    		
	    			entity(row)(row)=entity(row)(row)/subval
	    			lookcol=lookcol+1
	          
	    			if(lookcol>=width) check=false
	      }
	      	     
	    while(check){
	    		  
	    			if(lookcol==row){ } //何もしない
	    			
	    			else{
	    			  //実際の処理
	    			}
	    			lookcol=lookcol+1
	    			if(lookcol>=width) check=false
	    		}
	      
	    
	    row=row+1
	    //ループの最後に下の段へ移動
	    
	  }
	  
	  
	}
	
	


}
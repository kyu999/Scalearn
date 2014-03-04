package classifier

trait LinearRegression {
	def BatchGradientDescent(x:Vector[Double],raw:Vector[Double],α:Double)={
	  val m=raw.length
	  var tempθ0=36.0
	  var tempθ1=0.90
	  var θ0=5.0
	  var θ1=0.5
	  var check=true
	  val zipped=raw.zip(x)
	  while (check){
	    tempθ0=θ0-α*(zipped.map(a=>θ0+θ1*a._1-a._2).reduce((a,b)=>a+b))/m
	    tempθ1=θ1-α*(zipped.map(a=>a._1*(θ0+θ1*a._1-a._2)).reduce((a,b)=>a+b))/m
	    if(θ0==tempθ0 && tempθ1==θ1) check=false
	    θ0=tempθ0
	    θ1=tempθ1
	    println("tempθ0 : "+tempθ0+" , tempθ1 : "+tempθ1+" ,  θ0 : "+θ0+" , θ1 : "+θ1)
	  }
	}
}
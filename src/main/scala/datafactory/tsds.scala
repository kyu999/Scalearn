package datafactory

class tsds(tsdalist:Vector[tsda]) extends dase(tsdalist){
	
	val acf=tsdalist.map(_.acf)
	
//Operation
	
	def differencing=tsdalist.map(_.differencing)
	
}
 
object tsds{
  def apply(x:tsda*)= 
    x match{
    case v:Vector[tsda]=>new tsds(v)
    case s:Seq[tsda]=>new tsds(s.toVector)
  }
}
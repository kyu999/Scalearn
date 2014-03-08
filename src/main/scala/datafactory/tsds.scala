package datafactory

class tsds(tsdalist:Vector[tsda]) extends ds(tsdalist){
	
	val acf=tsdalist.map(_.acf)
	lazy val diff=tsdalist.map(_.diff)
	
}

object tsds{
  def apply(x:tsda*)=
    x match{
    case v:Vector[tsda]=>new tsds(v)
    case s:Seq[tsda]=>new tsds(s.toVector)
  }
}
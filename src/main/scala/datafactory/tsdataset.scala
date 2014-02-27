package datafactory

class tsdataset(raw:Seq[data]) extends dataset(raw){

}

object tsdataset{
  def apply(x:Seq[data])=new tsdataset(x)
}
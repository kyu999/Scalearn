package datafactory

import scala.math._

class infds(datalist:Vector[data]) extends dase(datalist){
	//Inference Dataset
	
	lazy val tpair:Vector[(Double,Boolean)]=combi.map(a=>paired_t_test(a(0).raw,a(1).raw))
	
	lazy val twelch:Vector[(Double,Boolean)]=combi.map(a=>welch_t_test(a(0).raw,a(1).raw))
	
	lazy val gsize=datalist.map(_.n).sum
	//grand size
    
    lazy val gsum=datalist.map(_.sum).sum
    //grand sum
    
    lazy val gmean=datalist.map(elt=>( elt.n*elt.mean ) ).sum / gsize 
    //grand mean : 各要因データサイズの重みでの各要因の平均の荷重平均。一般平均という。
    
    lazy val effects=datalist.map(elt=>elt.mean-gmean)
    //Ai水準の効果(effect)：αi=µi-µ　これらを鑑みると、y[ij]=µ+α[i]+ε[ij]
    
	lazy val ct=pow(gsum,2)/gsize
	//CT stands for correction term(修正項)
	
	lazy val gmeansquare=datalist.map(_.squaredsum).sum
	//Sr : grand mean square(総平方和) == Sa+Se：ΣiΣj(Xij^2)    ※iは要因数,jは各要因のサイズ
	
	lazy val Sa=datalist.map(elt=>pow(elt.sum,2)/elt.n).sum-ct
	//水準の変更に伴うデータの変動の大きさを表す、級間平方和
	
	lazy val Se=gmeansquare-Sa
	//同一実験条件化でのデータの変動の大きさを表す誤差平方和
	
}

object infds{
  def apply(x:data*)=new infds(x.toVector)

}
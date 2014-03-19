package datafactory

import scala.math._
import distribution.F

class infds(datalist:Vector[data]) extends dase(datalist){
	//Inference Dataset
	
	lazy val tpair:Vector[(Double,Boolean)]=combi.map(a=>paired_t_test(a(0).raw,a(1).raw))
	
	lazy val twelch:Vector[(Double,Boolean)]=combi.map(a=>welch_t_test(a(0).raw,a(1).raw))
	
	lazy val grandsize=datalist.map(_.n).sum
	//grand size
    
    lazy val grandsum=datalist.map(_.sum).sum
    //grand sum
    
    lazy val grandmean=datalist.map(elt=>( elt.n*elt.mean ) ).sum / grandsize 
    //grand mean : 各要因データサイズの重みでの各要因の平均の荷重平均。一般平均という。
    
    lazy val effects=datalist.map(elt=>elt.mean-grandmean)
    //Ai水準の効果(effect)：αi=µi-µ　これらを鑑みると、y[ij]=µ+α[i]+ε[ij]
    
	lazy val ct=pow(grandsum,2)/grandsize
	//CT stands for correction term(修正項)
	
	lazy val grandSS=datalist.map(_.squaredsum).sum-ct
	//Sr : grand square sum(総平方和) == Sa+Se：ΣiΣj(Xij^2)    ※iは要因数,jは各要因のサイズ
	
	lazy val factorSS=datalist.map(elt=>pow(elt.sum,2)/elt.n).sum-ct
	//Sa : 水準の変更に伴うデータの変動の大きさを表す、級間平方和
	
	lazy val errorSS=grandSS-factorSS
	//Se : 同一実験条件化でのデータの変動の大きさを表す誤差平方和
	
	lazy val factorsize=datalist.length
	//要因数

	lazy val factorDf:Double=factorsize-1
	//Va : 要因自由度
	
	lazy val errorDf:Double=grandsize-factorsize
	//Ve : 誤差自由度
	
	lazy val factorMS=factorSS/factorDf
	//factor mean square
	
	lazy val errorMS=errorSS/errorDf
	//error mean square
	
	lazy val fval=factorMS/errorMS
	//F値

	
//Operation	
	
	
	def anova:(Boolean,Double)=F.table(factorDf.toInt,errorDf.toInt,fval)
	
	/*
		def eachsubsum(in:data)={
	  (in:data)
	}
	lazy val factorSS2=datalist.map(
	    (elt:data)=>elt.subtotaling(raw)
	    andThen (elt:data)
	    )
	Sb : もう一つの要因に置ける級間平方和
	 * 
	 */
	
	
}

object infds{
  def apply(x:data*)=new infds(x.toVector)

}
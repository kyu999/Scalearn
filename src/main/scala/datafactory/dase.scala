package datafactory

import scala.math._
import math._

class dase(datalist:Vector[data]) extends Descritive 
			with Inference with Bayes with Multivariate{
  
  //datalistが欲しくなったらresolveで分解しよう。ただ、そもそもdataをdatasetに引数としていれてるんだからdataが欲しいとはあまりならないと思われる
  //datalist.map(~)を多用してしまっているがdatalistのリストはそれほど巨大にならないと想定している上に、各変数の統計量は既にdata内で算出されている。また、使用頻度の高くなさそうなものは遅延評価にしているため計算量的にそんなに問題ない。
            
    val raw:Vector[Vector[Double]]=datalist.map(a=>a.raw)
    
    val mean=datalist.map(_.mean)		

    val vari=datalist.map(_.vari)
    
    val sd=datalist.map(_.sd)
    
    lazy val samplevari=datalist.map(_.samplevari)
    
    lazy val samplesd=datalist.map(_.samplesd)
    
    lazy val combi:Vector[Vector[data]]=datalist.combinations(2).toVector
    //組み合わせ(2)
	
    lazy val covar=combi.map{a=>covariance(zipdevi(a(0).dv,a(1).dv))}.toVector
	//共分散
    
    lazy val pears=combi.map{a=>
      
       if(a(0).n != a(1).n) {println("You can't compare different length variable");-10000}
       else{
	   pearson(
	       covariance(
	           zipdevi(a(0).dv,a(1).dv) )
	           ,a(0).sd,a(1).sd)
       }
	   }.toVector
	
	   
	lazy val spears=combi.map{a=>
	  
	  if(a(0).n != a(1).n){println("You can't compare different length variable");-10000}
	  else{
	      spearman(difsqured(labeling(a(0).raw,a(1).raw)))
	     }
	  }
	   
	lazy val eucli=combi.map(a=>euclidean(a(0).raw,a(1).raw))

	lazy val time:Vector[Vector[Double]]=raw.map{a=>(1 to a.length).map(_.toDouble).toVector}
	

	   
	lazy val reg=combi.zip(pears).map{a=>regression(a._2,a._1(0).sd,a._1(1).sd,a._1(0).mean,a._1(1).mean)}
	//regressionの引数は順に、相関係数、XのSD,YのSD、Xの平均、Yの平均. output=(slope,intercept).計算にpearsonを使っているから異なる長さの変数には適用出来ない。注意して使うように。
	
	lazy val regline:Vector[Double=>Double]=reg.map(a=>{(x:Double)=>a._1*x+a._2})
	//共にregの値を基にした無名関数。xを与えてyを得る
	
	
//Operation	
	
	def inf=new infds(datalist)
	
	def ts=new tsds(datalist.map(a=>a.ts))
	//時系列データ化
	
	def naming(in:String*)=in.zip(datalist).foreach{a=>a._2.name=a._1} 
	def naming(in:Array[String])=in.zip(datalist).foreach{a=>a._2.name=a._1} 
	//side effect
  
	def resolve:Vector[data]=datalist
	//dsを分解しdaのVectoruenceを返す
	
	def tomat=new m(raw) 
	//def mat(direction)={データを行列に変換＝＝行列クラスのインスタンスを返す}
	
	def ::(component:data)=new dase(component+:datalist)
	//componentは末尾に追加される。既存のdsに新たな要素を１つ加えたい場合に使ってください。複数追加は効率悪いです
	//コンパニオンオブジェクトのapplyメソッドの引数はda*なのでこのままではエラーとなるから直接newでクラスを作ってる
	//既存のdsから新たなdaを１つ加えたdsを作る。効率に関して考える必要はある。複数追加する必要があるならds(....)を使うべき
	
    def summary={
        mkLine 
        println("name list : ")
        datalist.foreach(x=>println(x.name))
		mkLine
		println("length of each")
        datalist.foreach(a=>println(a.n))
        mkLine
		println("mean : ")
		mean.foreach(println)
		mkLine
		println("estimated sd : ")
		sd.foreach(println)
		mkLine
		println("combination : ")
		combi.foreach(a=>println(a(0).name+" & "+a(1).name))
		mkLine
		println("covariance : ")
		covar.foreach(println)
		mkLine
		println("pearson's correlation : ")
		pears.foreach(println)
		mkLine
		println("spearman's correlation : ")
		spears.foreach(println)
		mkLine        
	}
	
}

//コンパニオンオブジェクトを作成。applyでファクトリメソッドを定義しているのでnewが不要になる
object dase{
  
  def apply(datalist:data*):dase=new dase(datalist.toVector)
//  def apply(raws:Seq[Double]*):dase=new dase(raws.map(raw=>data(raw.toVector)).toVector)
  
}


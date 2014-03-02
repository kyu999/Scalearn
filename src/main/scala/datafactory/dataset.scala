package datafactory

class dataset(datalist:Seq[data]) extends Descritive 
			with Inference with Bayes with Multivariate{
		
    val raw:Seq[Seq[Double]]=datalist.map(a=>a.raw)
    
    val mean=datalist.map(_.mean)
    
    val sd=datalist.map(_.sd)
    
    
    lazy val combi:Seq[Seq[data]]=datalist.combinations(2).toVector
	
    lazy val covar=combi.map{a=>covariance(zipdevi(a(0).dv,a(1).dv))}.toVector
	
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

	lazy val time:Seq[IndexedSeq[Double]]=raw.map{a=>(1 to a.length).map(_.toDouble)}
	

	   
	lazy val reg=combi.zip(pears).map{a=>regression(a._2,a._1(0).sd,a._1(1).sd,a._1(0).mean,a._1(1).mean)}
	//regressionの引数は順に、相関係数、XのSD,YのSD、Xの平均、Yの平均. output=(slope,intercept).
	//計算にpearsonを使っているから異なる長さの変数には適用出来ない。注意して使うように。
	
	lazy val xregline:Seq[Double=>Double]=reg.map(a=>{(x:Double)=>a._1*x+a._2})
	lazy val yregline:Seq[Double=>Double]=reg.map(a=>{(y:Double)=>(y-a._2)/a._1})
//共にregの値を基にした無名関数。xreglineはxを与えてyを得る。yreglineはその逆.x=>yは１つだけど　y=>xは複数になりうるから実装しない方が良いかも
	
	
//Operation	
	
	
	def ts=new tsdataset(datalist.map(a=>a.ts))
	//時系列データ化
	
	def naming(in:String*)=in.zip(datalist).foreach{a=>a._2.name=a._1} 
	//side effect
  
	def resolve:Seq[data]=datalist
	//datasetを分解しdataのsequenceを返す
	
	//def mat(direction)={データを行列に変換＝＝行列クラスのインスタンスを返す}
	
	def ::(component:data)=new dataset(component+:datalist)
	//componentは末尾に追加される。既存のdatasetに新たな要素を１つ加えたい場合に使ってください。複数追加は効率悪いです
	//コンパニオンオブジェクトのapplyメソッドの引数はdata*なのでこのままではエラーとなるから直接newでクラスを作ってる
	//既存のdatasetから新たなdataを１つ加えたdatasetを作る。効率に関して考える必要はある。複数追加する必要があるならdataset(....)を使うべき
	
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
		println("sd : ")
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
object dataset{
  
  def apply(datas:data*):dataset=new dataset(datas)
//  implicit def apply(in:data*)=new DaInDs(in)
}

/*
class DaInDs(in:data*){
  def apply=new dataset(in.map(_.raw))
}
* 
*/


//applyメソッドの引数は可変長引数Seq[Double]*にして、class定義の方ではSeq[Seq[Double]]としている。
//こうすることでdata(x,y,z,a,b,c)みたいに書ける。

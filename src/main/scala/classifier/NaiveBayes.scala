package classifier

import scala.math._
import org.atilika.kuromoji._
import scala.collection.mutable.ListBuffer
import tokenfactory.jp

object NaiveBayes{	
  //並列コレクション化した方が効率上がるかもparメソッドの活用=>並列化によるオーバーヘッドが大きいため現状では使わないほうがいい。
  var data:ListBuffer[(Boolean,String)]
		  =VirtualData.datalist
  //元の値達は学習データ。data,Sdata,Fdataはmutable。falseの学習データには失敗しか入ってないけどtrueの方に可愛と成功が入ってる。これによって失敗という単語の重みは成功や可愛という単語よりも大きくなる。故に可愛いと失敗が１語づつのStringを調査するときは失敗に傾く。
    
  var Sdata=data.filter(x=>x._1==true)  //成功data。mutable
  var Fdata=data.filter(x=>x._1==false) //失敗data。mutable
  
  def preS:Double=log((Sdata.length-1).toDouble/data.length)	//examineでSdataには最初に調べる項目が追加されるがdata自体には関数の最後に追加される。それによって成功事例がおおく見積もられるのを防ぐためにSdataに-1をしてる
  def preF=log((Fdata.length-1).toDouble/data.length)
  //成功、失敗それぞれの事前確率。examineの中で使う。ただし追加したpostが影響しないようにvalを使おうと思ったけどdata数とSdata数との関係が崩れるからやめた
  
  def getString(alldata:ListBuffer[(Boolean,String)])=alldata.map(x=>x._2).mkString
  def stringSdata=getString(Sdata)	//成功例の文字列データ
  def stringFdata=getString(Fdata)	//失敗例の文字列データ
  
  def getToken(stdata:String)=jp.mkTokenS(stdata)
  def tokenSdata=getToken(stringSdata)		//成功例のトークンリスト
  def tokenFdata=getToken(stringFdata)		//失敗例のトークンリスト
  
  def getnum(tokens:Stream[Token])=tokens.length
  def numTokenS:Double=getnum(tokenSdata)
  def numTokenF:Double=getnum(tokenFdata)
  
  def getwords(tokens:Stream[Token]):List[String]=tokens.map(x=>x.getBaseForm).toList //後でListはStreamに直す
  def Swords=getwords(tokenSdata)
  def Fwords=getwords(tokenFdata)
  
  def WordPro(words:List[String],num:Double):Map[String,Double]=words.groupBy(x=>x).map(x=>(x._1,log(x._2.length/num)))   //logを挟む；出現頻度/単語の数
  def Swp=WordPro(Swords,numTokenS)
  def Fwp=WordPro(Fwords,numTokenF)
  
  def allyudo(proList:List[Double]):Double=proList.reduce((a,b)=>a+b) //本来a*bだがlogの計算なのでa+b；log(a*b)=log(a)+log(b)
    
  def time(f:String=>Unit,post:String)={
    val start=System.currentTimeMillis
    f(post)
    val end=System.currentTimeMillis
    println("it takes "+(end-start))
  }

  def examine(post:String):Unit={
	  //長いpostだと桁数漏れを起こす=>各尤度と事前確率に対してlogを取る
	    
    val Snow=(true,post)
    Snow+=:Sdata
    val Fnow=(false,post)
    Fnow+=:Fdata
    //両方にadd
    
    val dic=getToken(post).map(x=>x.getBaseForm).toSet.toList  //postの重複のなトークンリスト
    
    val yudoSList=dic.map(x=>Swp(x))
    val yudoFList=dic.map(x=>Fwp(x))
    
    val Saft=allyudo(yudoSList)*preS
    val Faft=allyudo(yudoFList)*preF
 
    summary
    
    if(Saft>Faft){
    		println("成功するでしょう") 
    		Fdata-=Fnow
    		Snow+=:data
    }
    else{
    		println("失敗するでしょう")
    		Sdata-=Snow
    		Fnow+=:data
    } 
    
    def summary={
    		println("成功事例 : "+Sdata)
    		println("失敗事例 : "+Fdata)
    		println("重複しない単語リスト : "+dic)   
    		println("成功時各尤度log : "+yudoSList)
    		println("失敗時各尤度log : "+yudoFList)
    		println("成功事前確率log : "+preS)
    		println("失敗事前確率log : "+preF)
    		println("成功事後確率log : "+Saft+" <=> 失敗事後確率log : "+Faft)
     }

   }
}
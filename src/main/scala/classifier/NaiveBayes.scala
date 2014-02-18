package classifier

import org.atilika.kuromoji._
import scala.collection.mutable.ListBuffer
import tokenfactory.kuro

object Tes extends App{
  val t=NaiveBayes.time _
  val f=NaiveBayes.examine _  //オブジェクトのメソッドは関数ではないので関数化するには　メソッド名+ _　 
  t(f,"失敗成功")
  println("")
  t(f,"彼は私に気があるのでしょうか？") 
  println("")
  t(f,"""
もしうまくいっても、４月から私は東京に戻ってしまいます。
それに教習所の先生はもてますよね…。
私は今まで誰かと付き合ったことがないのでよくわかりません。
これは、やめておいた方がいい恋なのでしょうか？""")
 
}
object NaiveBayes{	
  //並列コレクション化した方が効率上がるかもparメソッドの活用。
  var data:ListBuffer[(Boolean,String)]
		  =ListBuffer((true,"成功"),(false,"失敗"))
		  //元の値達は学習データ。data,Sdata,Fdataはmutable。falseの学習データには失敗しか入ってないけどtrueの方に可愛と成功が入ってる。これによって失敗という単語の重みは成功や可愛という単語よりも大きくなる。故に可愛いと失敗が１語づつのStringを調査するときは失敗に傾く。
    
  var Sdata=data.filter(x=>x._1==true)  //成功data。mutable
  var Fdata=data.filter(x=>x._1==false) //失敗data。mutable
  
  def preS:BigDecimal=(Sdata.length-1).toDouble/data.length	//examineでSdataには最初に調べる項目が追加されるがdata自体には関数の最後に追加される。それによって成功事例がおおく見積もられるのを防ぐためにSdataに-1をしてる
  def preF=1-preS
  //成功、失敗それぞれの事前確率。examineの中で使う。ただし追加したpostが影響しないようにvalを使おうと思ったけどdata数とSdata数との関係が崩れるからやめた
  
  def getString(alldata:ListBuffer[(Boolean,String)])=alldata.map(x=>x._2).mkString
  def stringSdata=getString(Sdata)	//成功例の文字列データ
  def stringFdata=getString(Fdata)	//失敗例の文字列データ
  
  def getToken(stdata:String)=kuro.mkTokenS(stdata)
  def tokenSdata=getToken(stringSdata)		//成功例のトークンリスト
  def tokenFdata=getToken(stringFdata)		//失敗例のトークンリスト
  
  def getnum(tokens:Stream[Token])=tokens.length
  def numTokenS:BigDecimal=getnum(tokenSdata)
  def numTokenF:BigDecimal=getnum(tokenFdata)
  
  def getwords(tokens:Stream[Token]):List[String]=tokens.map(x=>x.getBaseForm).toList //後でListはStreamに直す
  def Swords=getwords(tokenSdata)
  def Fwords=getwords(tokenFdata)
  
  def WordPro(words:List[String],num:BigDecimal):Map[String,BigDecimal]=words.groupBy(x=>x).map(x=>(x._1,x._2.length/num))
  def Swp=WordPro(Swords,numTokenS)
  def Fwp=WordPro(Fwords,numTokenF)
  
  def allyudo(proList:List[BigDecimal]):BigDecimal=proList.reduce((a,b)=>{println("a : "+a+", b : "+b);a*b}) 
    
  def time(f:String=>Unit,post:String)={
    val start=System.currentTimeMillis
    f(post)
    val end=System.currentTimeMillis
    println("it takes "+(end-start))
  }
  def examine(post:String):Unit={
	    
    val Snow=(true,post)
    Snow+=:Sdata
    val Fnow=(false,post)
    Fnow+=:Fdata
    //両方にadd
    println("成功事例 : "+Sdata)
    println("失敗事例 : "+Fdata)
    
    val dic=getToken(post).map(x=>x.getBaseForm).toSet.toList  //postの重複のなトークンリスト
    println("重複しない単語リスト : "+dic)
    
    val yudoSList=dic.map(x=>Swp(x))
    println("成功時各尤度 : "+yudoSList)
    val yudoFList=dic.map(x=>Fwp(x))
    println("失敗時各尤度 : "+yudoFList)

    println("成功事前確率 : "+preS)
    println("失敗事前確率 : "+preF)
    println("Saft start")
    
    val Saft=allyudo(yudoSList)*preS
    println("Faft start")
    val Faft=allyudo(yudoFList)*preF
    println("成功事後確率 : "+Saft+" <=> 失敗事後確率 : "+Faft)
    //長いpostだと桁数漏れを起こす=>BigDecimalを使う
    
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
    
  }
}
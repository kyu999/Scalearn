package supervised

import scala.math._
import org.atilika.kuromoji._
import scala.collection.mutable.ListBuffer
import tokenfactory.JP
import org.apache.spark.rdd._


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
  
  def getToken(stdata:String)=JP.mkToken(stdata)
  def tokenSdata=getToken(stringSdata)		//成功例のトークンリスト
  def tokenFdata=getToken(stringFdata)		//失敗例のトークンリスト
  
  def getnum(tokens:Seq[Token])=tokens.length
  def numTokenS:Double=getnum(tokenSdata)
  def numTokenF:Double=getnum(tokenFdata)
  
  def getwords(tokens:Seq[Token]):List[String]=tokens.map(x=>x.getBaseForm).toList //後でListはSeqに直す
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

/*
    val Snow=(true,post)			
    Snow+=:Sdata
    val Fnow=(false,post)
    Fnow+=:Fdata
    //両方にadd
*/
    
    val dic=getToken(post).map(x=>x.getBaseForm).toSet.toList  //postの重複のなトークンリスト
    
    val yudoSList=dic.map(x=>Swp(x))
    val yudoFList=dic.map(x=>Fwp(x))
    
    val Saft=allyudo(yudoSList)*preS
    val Faft=allyudo(yudoFList)*preF
 
    summary
    
    if(Saft>Faft){
    		println("成功するでしょう") 
//    		Fdata-=Fnow
//    		Snow+=:data
    }
    else{
    		println("失敗するでしょう")
//    		Sdata-=Snow
//    		Fnow+=:data
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


import org.apache.spark.rdd._
import scala.math.log
import io.read
import java.io.Serializable

/**
input : Vector[ (the class name the document belong to , path to document ) ]
**/

case class NaiveBayesian( file_pathes : Vector[(String,String)] ) extends Serializable 
{
    /** both strings as inputs above stand for classes here **/
	
	val docs:Vector[(String ,RDD[(String,Int)])] = file_pathes.map( class_path => ( class_path._1 , read.rdds(class_path._2,false) ) )
	
	val wholeClass :Map[String,Vector[(String,RDD[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> Vector( ( 各クラス,ドキュメントの集合 ) ) )
	
	val allClassNames:Vector[String] = wholeClass.map(elt=>elt._1).toVector
	//全classのリスト作成
	
	val eachNumDocs:Map[String,Int] = wholeClass.map(elt=>(elt._1,elt._2.length))
	//Nc：各クラスに置けるdocument数、のMap : Map(class->number of docs)
	
	val sumNumDocs:Int = docs.size
	//ΣNc：総document数
	
	def eachNumWord(word:String , class_name:String ):Int = {

		println(" eachNumWord---------------------------------------------")
				
		var doc_count = 0

		wholeClass(class_name).foreach{
			class_rdd =>  // == (class,rdd)
				val filtered = class_rdd._2.filter{word_occur=> word_occur._1==word}.take(1)
				if(filtered.size!=0) doc_count += 1
		}
		
		println("eachNumWord Done!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
		doc_count
	}
	//N(w,c)：各クラスに置ける各単語の出現回数、のMap。できれば。もし未知語が来たらNoneではなく0を返す
	
	def eachProbWord(word:String , class_name:String , alpha:Int = 2):Double={
		println(" eachProbWord---------------------------------------------")
		
		val Nwc = eachNumWord(word , class_name).toDouble
		val Nc = eachNumDocs(class_name).toDouble
		
		println("Nwc : "+Nwc)
		println("Nc : "+Nc)
		
		log( ( Nwc+(alpha-1) ) / ( Nc + 2*(alpha-1) ) )
	}
	//log(Pw,c)
	//alpha is the parameter to decide how much we gonna make the data flat
	
	def eachProbClass(class_name:String):Double={
		println(" eachProbClass---------------------------------------------")
		
		val Nc = eachNumDocs(class_name).toDouble
		
		log( ( Nc+1 ) / ( sumNumDocs + NumClass ) )

	}
	
	val NumClass = wholeClass.size
	//|C|：クラスの種類数
	
	def temp(doc_path:String) ={
	
		val cached_rdd = read.rdds(doc_path)
		
		println ( "cached_rdd : "+cached_rdd.collect() ) 	
		println ( "cached_rdd : "+cached_rdd.collect() ) 	//okならactionの回数は問題ではない
		
		val each_prob : Vector[RDD[Double]] =
			allClassNames.map{
				class_name => 	
						cached_rdd
							.map { elt => eachProbWord(elt._1 , class_name ) * elt._2 }
			}
			
		each_prob
		
//		each_prob.map(_.take(1))

//					val sum_prob  = each_prob.reduce{ (a,b) => a+b } 
			
	}
	

	def classify(doc_path:String , alpha:Int = 2 ) = {

		val cached_rdd = read.rdds(doc_path)	//何度も使うのでcache化
								
		val ProbPerClass = 
			allClassNames.map{
				class_name => 					
										
					val each_prob = 
						cached_rdd.map { elt => eachProbWord(elt._1 , class_name , alpha) * elt._2 }
										
					val sum_prob : Double = each_prob.reduce{ (a,b) => a+b } 
										
					sum_prob + eachProbClass(class_name)
				}	
		//list of probability that this document would belong to
		println(" max_class---------------------------------------------")

		println("ProbPerClass : "+ProbPerClass)
		
		val max_class : (Double,Int) = ProbPerClass.zipWithIndex.max
		// ( probability , index of the class )
		println(" return estimation class---------------------------------------------")
		
		allClassNames(max_class._2)
		//推定クラスを返す
		
		
		/**
		cached_rddの各要素：(word,frequency) => probability of the word * frequency in the document 
		各クラスに置ける各単語の頻度を返したい：Vector[RDD] ※ただしrddは単語ではなく確率を返す
		rddに含まれる全ての単語を、分類器を使って各クラス各単語出現確率(log)へ変換 
		reduceで各確率を足し合わせ、log事前確率P(c)を足したものをクラスごとに作成
		最も高いクラスを推定クラスとして(String,rdd)という形で既存の分類器にデータを追加
		**/
	}
		
}

object DoNaiveBaysian extends App{

  	val file_pathes = 
  		Vector( ("plus","resource/doc1.txt"),
  			    ("plus","resource/doc2.txt"),
  			    ("plus","resource/doc3.txt"),
  			    ("minus","resource/doc4.txt"),
  			    ("minus","resource/doc5.txt"),
  			    ("minus","resource/doc6.txt")
  			  )
  	
  	val pn = ParallelNaive(file_pathes)  	

 	val cached_rdd = read.rdds("resource/examine.txt")	
 	     
 	val each_prob : Vector[RDD[String]] =
		pn.allClassNames.map{	
			class_name => 	
					cached_rdd
						.map { elt => ( pn.eachProbWord(elt._1 , class_name ) * elt._2 ).toString }
			}
     
	val head_prob = each_prob.head
	
	println("head_prob : "+head_prob.take(1).head)
	
    println(pn.docs.map(elt=>elt._2.take(1).head))
      
      
      
     
//	pn.temp("resource/examine.txt")
}
package tokenfactory

import org.atilika.kuromoji._
import scala.collection.JavaConversions._
import scala.io.Source
object  kuro{
	
	def mkTokenL(content:String):List[Token]=Tokenizer.builder().build().tokenize(content).toList.filter((x:Token)=>x.getBaseForm() != null)
	//StringからToken作成
	def mkTokenS(content:String):Stream[Token]=Tokenizer.builder().build().tokenize(content).toStream.filter((x:Token)=>x.getBaseForm() != null)
	
	def mkTokenF(path:String):Stream[Token]={
	  val file=Source.fromFile(path)
	  val content=file.getLines.mkString
	  mkTokenS(content)
	}
	//File pathからToken作成
	
	def nonrepeatW(Stream:Stream[Token]):Stream[String]={
	  Stream
	  .groupBy(x=>x.getBaseForm)
	  .values
	  .toStream
	  .map(x=>x(0).getBaseForm)
	}
	//Tokenリストから重複のない単語リスト作成
	
	def base(Stream:Stream[Token]):Stream[String]=Stream.map((x:Token)=>x.getBaseForm)
	//Tokenのリストから基の単語リストを取得
	
	def reading(Stream:Stream[Token]):Stream[String]=Stream.map((x:Token)=>x.getReading())
	//Tokenのリストから読みの単語リストを取得
	
	def resolverF(path:String):Stream[String]=mkTokenF(path).map((x:Token)=>x.getBaseForm())
	//Fileのパスから元の単語リストを取得
	
	def count(word:String)(tokens:Stream[Token]):Double={
	  tokens match{
	    case Stream.Empty=>0
	    case Stream(x)=>if(x.getBaseForm==word) 1 else 0
	    case x#::xs=>
	      if(x.getBaseForm==word) 1+count(word)(tokens.tail)
	      else count(word)(tokens.tail)
	  }
	 }
	//Tokenリストの中に含まれるかもしれない特定の単語の出現回数をカウント
	
	def sortCount(path:String):Stream[String]={
			mkTokenF(path)
			.groupBy(x=>x.getBaseForm)
	  		.values
	  		.toStream.sortWith(_.length>_.length)
	  	    .map(x=>"count: " +x.length+" "+x(0).getBaseForm())
	}
	//File pathからTokenリストを作成し出現頻度でソート

	
//---------------------------------------------------カリー化や合成の試し
	
	
	def condition(cond:Token=>Boolean)=
	  sortCount("/Users/kyu/Documents/fruits/Exercise/src/textmining/shayo.txt")
	
	def oppcond(limit1:String)(limit2:String)=condition((x:Token)=>(!(x.getPartOfSpeech().startsWith(limit1)) && !(x.getPartOfSpeech().startsWith(limit2))))
	
//	oppcond("助詞")("動詞")
	
//	println("")
	
	def removenoun(limit:String)=oppcond(limit)("名詞")
//	println("")
//	println("f start")
//	val does=removenoun("動詞")
//	does
}
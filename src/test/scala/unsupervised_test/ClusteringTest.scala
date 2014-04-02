package unsupervised_test
 
import tokenfactory.JP
import org.atilika.kuromoji._
import preprocessing._
import org.scalatest.FunSuite
import datafactory._
import datafactory.Converter._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._
import preprocessing.WordsConverter

class ClusteringTest extends FunSuite{
/*	
	val tokens=JP.mkTokenV("鋭利可愛いよ！可愛いすぎるよ…くぁいい。でもこれからの展開も気になる所です。")
	val tokens2=JP.mkTokenV("この巻は今までの巻と比べても毛並みが違って好き嫌いが分かれるところじゃないかな。まず、重い。雰囲気もそうだしストーリー自体も前巻を引きずってスタートしとる。この巻で初めて八幡は自身の手法の限界にぶちあたりやり方を今までと変え、ある種進歩したと言える。少なくとも由比ケ浜にとってはそれが進歩に見えたことなんだろうが皮肉にも雪ノ下の考えは推測するにみんなで生徒会に入り負担を分散し今までの関係を維持するところにあったんじゃないかな。悲しいことにやり方を変えたにしろすれ違ってしまう八幡＆由比ケ浜ルート。")
	val allwords=JP.mkTokenV("ご覧になって！あの一面の花畑を！！ご覧なさい！君の目は節穴か！鋭利がひたすら可愛い！！表紙通り鋭利回だったわけだけどちょっと物足りなさも感じた１巻でもあった。バトルが全体的に迫力なかったね。1,2巻までは良かったけど3巻からどうもバトルはビミョーになってる気がする。まあ鋭利が可愛いからいいけどね！ 甘酸っぱすぎて高校生やり直したいけどやり直しても絶対こうならないよね！笑　なにもーこれ。もーーー。続きはやくよみてーー！！これは悶えるわ。くそーーくそーー！かっこかわいいよおおおおお この巻は今までの巻と比べても毛並みが違って好き嫌いが分かれるところじゃないかな。まず、重い。雰囲気もそうだしストーリー自体も前巻を引きずってスタートしとる。この巻で初めて八幡は自身の手法の限界にぶちあたりやり方を今までと変え、ある種進歩したと言える。少なくとも由比ケ浜にとってはそれが進歩に見えたことなんだろうが皮肉にも雪ノ下の考えは推測するにみんなで生徒会に入り負担を分散し今までの関係を維持するところにあったんじゃないかな。悲しいことにやり方を変えたにしろすれ違ってしまう八幡＆由比ケ浜ルート。").map(elt=>elt.getBaseForm).toSet
	val fv=FrequencyVector(tokens,allwords) 
	val fv2=FrequencyVector(tokens2,allwords)
	val vc=VectorCluster(Vector(fv,fv2,fv,fv,fv))
	val vc2=VectorCluster(Vector(fv,fv2,fv2,fv2,fv2))
	println("old cluster : "+vc.size)
	println("new cluster : "+(vc++vc2).center.length)
	val vczip=vc.center.zip(fv.values)
//	println(fv.freqs)
	val dasevc=dase(vc.center.toVector.toda,fv2.values.toVector.toda)
	println("similarity : "+dasevc.pears)
	println("fv1 : "+fv.values)
	println("fv2 : "+fv2.values)
	
	val d1=dase(fv.values.toda,fv2.values.toda)
	println(d1.simcos)
//	println(allwords)
 * 
 */
/*
	val tokens3=JP.mkTokenV("私はリンゴとてもとてもが食べたい")
	val tokens4=JP.mkTokenV("私はゴリラが食べたい")
	val tokens5=JP.mkTokenV("""私はゴリラです（計算機科学） 多数の低廉なコンピュータを、特別なソフトウェア・ハードウェアを用いて、あたかも1つの大きなコンピュータとして利用できるように接続すること。
ノードをリンクするのに使われるハードウェアは通常相互接続（interconnect）と呼ばれ、ネットワークインタフェースカード（Network Interface Card、NIC）、スイッチ及びケーブルによって構成されている。どのような相互接続を選択するかは様々な要因に拠る。ノード同士が頻繁に通信しなければならないような細粒度アプリケーションの為には、相互接続は十分高速でなければならない。
ソフトウェアの点からみれば、クラスタリングは信頼性を得る為（1台のマシンが動作しなくなっても他がその作業を引き継ぐ）、またはコンピュータの非常に高い性能を廉価に得る1手段として利用される。コンピュータ・クラスターを参照。
（統計学）データ解析手法の1つ。機械学習やデータマイニング、パターン認識、イメージ解析やバイオインフォマティックスなど多くの分野で用いられる。クラスタリングではデータの集合を部分集合（クラスタ）に切り分けて、それぞれの部分集合に含まれるデータが（理想的には）ある共通の特徴を持つようにする。この特徴は多くの場合、類似性や、ある定められた距離尺度に基づく近さで示される。データ・クラスタリングを見よ。
（人口統計学）民族、経済、信仰などに基づいた様々な人口の集合体。""")
	val alltypes=JP.mkTokenV("""私はとてもリンゴゴリラが食べたいです（計算機科学） 多数の低廉なコンピュータを、特別なソフトウェア・ハードウェアを用いて、あたかも1つの大きなコンピュータとして利用できるように接続すること。
ノードをリンクするのに使われるハードウェアは通常相互接続（interconnect）と呼ばれ、ネットワークインタフェースカード（Network Interface Card、NIC）、スイッチ及びケーブルによって構成されている。どのような相互接続を選択するかは様々な要因に拠る。ノード同士が頻繁に通信しなければならないような細粒度アプリケーションの為には、相互接続は十分高速でなければならない。
ソフトウェアの点からみれば、クラスタリングは信頼性を得る為（1台のマシンが動作しなくなっても他がその作業を引き継ぐ）、またはコンピュータの非常に高い性能を廉価に得る1手段として利用される。コンピュータ・クラスターを参照。
（統計学）データ解析手法の1つ。機械学習やデータマイニング、パターン認識、イメージ解析やバイオインフォマティックスなど多くの分野で用いられる。クラスタリングではデータの集合を部分集合（クラスタ）に切り分けて、それぞれの部分集合に含まれるデータが（理想的には）ある共通の特徴を持つようにする。この特徴は多くの場合、類似性や、ある定められた距離尺度に基づく近さで示される。データ・クラスタリングを見よ。
（人口統計学）民族、経済、信仰などに基づいた様々な人口の集合体。""").map(elt=>elt.getBaseForm).toSet
	val fv3=FrequencyVector(tokens3,alltypes) 
	val fv4=FrequencyVector(tokens4,alltypes) 
	val fv5=FrequencyVector(tokens5,alltypes) 

	val input_data=ArrayBuffer(fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv4.values,fv5.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv3.values,fv5.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values,fv4.values,fv5.values,fv3.values)
	 //3453545335453=>33333 444 55555
//	println("alltypes : "+alltypes)
	println("size : "+input_data.length)
	val data=unsupervised.Clustering.kmeans(3, input_data)
	println("data : "+data)
	data.foreach(cluster=>cluster.freq_vectors.map(array=>println(array.zip(alltypes).map{vector=>if(vector._1>0) vector._2 else "" })))
	* 
	*/
	
/*	val easy_data=data.map(elt=>elt.freq_vectors.map{each=>
	  val zipped=each.zip(alltypes)
	 
	  zipped.map(vector=>if(vector._1>0) vector._2 else "")
	})
	easy_data.foreach{elt=>println("");println(elt)}
	* 
	*/
//	println("clusters : ")
/*
	println("after : "+data)
	println("before : "+input_data)
	*/
  
	println("めっちゃ疎なデータ："+WordsConverter.toMathVector("りんこりんかわいいよおお", """りんこりんかわいいよおお点からみれば、クラスタリングは信頼性を得る為（1台のマシンが動作しなくなっても他がその作業を引き継ぐ）、またはコンピュータの非常に高い性能を廉価に得る1手段として利用される。コンピュータ・クラスターを参照。
（統計学）データ解析手法の1つ。機械学習やデータマイニング、パターン認識、イメージ解析やバイオインフォマティックスなど多くの分野で用いられる。クラスタリングではデータの集合を部分集合（クラスタ）に切り分けて、それぞれの部分集合に含まれるデータが（理想的には）ある共通の特徴を持つようにする。この特徴は多くの場合、類似性や、ある定められた距離尺度に基づく近さで示される。データ・クラスタリングを見よ。
（人口統計学）民族、経済、信"""))
  
	

}
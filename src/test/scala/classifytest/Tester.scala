package classifytest
 
import tokenfactory.JP
import org.atilika.kuromoji._
import preprocessing._
import org.scalatest.FunSuite
import datafactory._
import Converter._


class Tester extends FunSuite{
	
	val tokens=JP.mkTokenV("鋭利可愛いよ！可愛いすぎるよ…くぁいい。でもこれからの展開も気になる所です。")
	val tokens2=JP.mkTokenV("この巻は今までの巻と比べても毛並みが違って好き嫌いが分かれるところじゃないかな。まず、重い。雰囲気もそうだしストーリー自体も前巻を引きずってスタートしとる。この巻で初めて八幡は自身の手法の限界にぶちあたりやり方を今までと変え、ある種進歩したと言える。少なくとも由比ケ浜にとってはそれが進歩に見えたことなんだろうが皮肉にも雪ノ下の考えは推測するにみんなで生徒会に入り負担を分散し今までの関係を維持するところにあったんじゃないかな。悲しいことにやり方を変えたにしろすれ違ってしまう八幡＆由比ケ浜ルート。")
	val allwords=JP.mkTokenV("ご覧になって！あの一面の花畑を！！ご覧なさい！君の目は節穴か！鋭利がひたすら可愛い！！表紙通り鋭利回だったわけだけどちょっと物足りなさも感じた１巻でもあった。バトルが全体的に迫力なかったね。1,2巻までは良かったけど3巻からどうもバトルはビミョーになってる気がする。まあ鋭利が可愛いからいいけどね！ 甘酸っぱすぎて高校生やり直したいけどやり直しても絶対こうならないよね！笑　なにもーこれ。もーーー。続きはやくよみてーー！！これは悶えるわ。くそーーくそーー！かっこかわいいよおおおおお この巻は今までの巻と比べても毛並みが違って好き嫌いが分かれるところじゃないかな。まず、重い。雰囲気もそうだしストーリー自体も前巻を引きずってスタートしとる。この巻で初めて八幡は自身の手法の限界にぶちあたりやり方を今までと変え、ある種進歩したと言える。少なくとも由比ケ浜にとってはそれが進歩に見えたことなんだろうが皮肉にも雪ノ下の考えは推測するにみんなで生徒会に入り負担を分散し今までの関係を維持するところにあったんじゃないかな。悲しいことにやり方を変えたにしろすれ違ってしまう八幡＆由比ケ浜ルート。").map(elt=>elt.getBaseForm).toSet
	val fv=FrequencyVector(tokens,allwords)
	val fv2=FrequencyVector(tokens2,allwords)
//	println(fv.freqs)
	println("fv1 : "+fv.values)
	println("fv2 : "+fv2.values)
	
	val d1=dase(fv.values.toda,fv2.values.toda)
	println(d1.simcos)
//	println(allwords)
  
}
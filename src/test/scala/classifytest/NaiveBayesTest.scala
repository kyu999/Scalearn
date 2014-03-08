package classifytest
import org.scalatest.FunSuite

import classifier.NaiveBayes

class NaiveBayesTest extends FunSuite{
  
  val t=NaiveBayes.time _
//  val f=NaiveBayes.examine _  //オブジェクトのメソッドは関数ではないので関数化するには　メソッド名+ _　 
//  VirtualData.examlist.map(x=>f(x))
  
}
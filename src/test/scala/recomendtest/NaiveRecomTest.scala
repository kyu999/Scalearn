package recomendtest

import org.scalatest.FunSuite	

import recommendation.NaiveRecom 

class NaiveRecomTest extends FunSuite{
  
  val data=NaiveRecom.data
  val topmat=NaiveRecom.topMatches _ 
  val pe=NaiveRecom.pearSP _ 
  val eu=NaiveRecom.euclidean _ 
  
  test("topMatch"){
  
  println("pearson's similarity : "+topmat(data, "Toby", 3, pe ))
  println("euclidean's similarity : "+topmat(data, "Toby", 3, eu ))
  
  }
  
}
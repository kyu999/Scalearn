package recomendtest

import org.scalatest.FunSuite	
import recommendation.NaiveRecom 

class NaiveRecomTest extends FunSuite{
  
  val data=NaiveRecom.data
  val topmat=NaiveRecom.topMatches _ 
  val pe=NaiveRecom.pearsim _ 
  val eu=NaiveRecom.euclisim _ 
  
  test("topMatch"){
  
  assert(topmat(data, "Toby", 3, pe )===List((0.9912407071619301,"Lisa Rose"), (0.924473451641905,"Mick LaSalle"), (0.8934051474415642,"Claudia Puig")),"pearson's similarity fail")
  assert(topmat(data, "Toby", 3, eu )===List((0.4,"Mick LaSalle"), (0.3567891723253309,"Claudia Puig"), (0.3483314773547883,"Lisa Rose")),"euclidean's similarity fail")
  
  }
  
}
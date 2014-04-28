package scalearn.probability.graphicalmodel

import scala.collection.mutable.{Set,Map}

    
object StudentExample extends App{

    val difficulty = 
        CpdFactor("difficulty", 
                     Vector(
                         Event("~60",0.1),      // <- ここらはデータから学習する
                         Event("60~70",0.2),
                         Event("70~80",0.3),
                         Event("80~90",0.3),
                         Event("90~",0.1)
                        )
                    )
        
    val iq = CpdFactor("iq",
                     Vector(
                         Event("~100",0.1),
                         Event("100~120",0.4),
                         Event("120~160",0.4),
                         Event("160~",0.1)
                        )
                    )
        
    val grade = CpdFactor("grade",
                    Vector(
                        Event("F",0.05),
                        Event("D",0.15),
                        Event("C",0.4),
                        Event("B",0.3),
                        Event("A",0.1)
                    )
                )

    val sat = CpdFactor("sat",
                    Vector(
                        Event("F",0.05),
                        Event("D",0.15),
                        Event("C",0.4),
                        Event("B",0.3),
                        Event("A",0.1)
                    )
                )
        
    val letter = CpdFactor("letter",
                    Vector(
                        Event("Pass",0.5),
                        Event("Fail",0.5)
                    )
                )
        
        
    val relation1 = DirectedRelation(difficulty.name,grade.name)
    val relation2 = DirectedRelation(iq.name,grade.name)
    val relation3 = DirectedRelation(iq.name,sat.name)
    val relation4 = DirectedRelation(grade.name,letter.name)
        
    val factors = Set(difficulty,iq,grade,sat,letter)
    
    val relations = Set(relation1,relation2,relation3,relation4)
        
    val bn = SimpleBayesianNetwork(factors,relations)
        
    println(bn)
        
        
        
}
package scalearn.probability.graphicalmodel
    
import scala.collection.mutable.{Set,Map}
    
    trait Network[ Re<:Relation , Fa<:Factor ] {
                        
        val relations : Set[Re]
        //mutable Set

        val factors : Set[Fa] 
            
        val factorReference :Map[String,Fa] = //mutable Map
            Map() ++
            ( factors
                .map{ factor => ( factor.name , factor ) }
                .toMap )
        
              
        def addFactor(factor:Fa) = {
            factors += factor
            factorReference += factor.name -> factor
        }
        
        def addRelation(relation:Re) = 
            relations += relation

        def removeFactor(factor:Fa) = {
            factors -= factor
            factorReference -= factor.name
        }
                
        def removeRelation(relation:Re) = 
            relations -= relation
            
        
            
    }


    trait BayesianNetwork[Fa<:Factor] extends Network[DirectedRelation,Fa] {
            
        val factors:Set[Fa]
                
        val relations:Set[DirectedRelation]
                
    }
           

    case class SimpleBayesianNetwork(
        
        factors:Set[CpdFactor],
        relations:Set[DirectedRelation]
        
    ) extends BayesianNetwork[CpdFactor]
        
        
    trait MarkovNetwork[ Ur<:UndirectedRelation, Fa<:Factor ] 
        extends Network[Ur,Fa] {
            
        val factors:Set[Fa]
                
        val relations:Set[Ur]
                 
    }

        
   case class PairwiseMarkovNetwork(
       
        factors:Set[NaiveFactor],
        relations:Set[PairwiseRelation]
       
    ) extends MarkovNetwork[PairwiseRelation,NaiveFactor]

        

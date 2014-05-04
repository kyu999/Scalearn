package scalearn.probability.graphicalmodel
    
import scala.collection.mutable.{Set,Map}
    
    trait Network[ De<:Dependency , Fa<:Factor ] {
                        
        val dependencies : Set[De]
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
        
        def addDependency(dependency:De) = 
            dependencies += dependency

        def removeFactor(factor:Fa) = {
            factors -= factor
            factorReference -= factor.name
        }
                
        def removeDependency(dependency:De) = 
            dependencies -= dependency
            
        
            
    }


    trait BayesianNetwork[Fa<:Factor] extends Network[DirectedDependency,Fa] {
            
        val factors:Set[Fa]
                
        val dependencies:Set[DirectedDependency]
                
    }
           

    case class SimpleBayesianNetwork(
        
        factors:Set[CpdFactor],
        dependencies:Set[DirectedDependency]
        
    ) extends BayesianNetwork[CpdFactor]
        
        
    trait MarkovNetwork[ Ud<:UndirectedDependency, Fa<:Factor ] 
        extends Network[Ud,Fa] {
            
        val factors:Set[Fa]
                
        val dependencies:Set[Ud]
                 
    }

        
   case class PairwiseMarkovNetwork(
       
        factors:Set[NaiveFactor],
        dependencies:Set[PairwiseDependency]
       
    ) extends MarkovNetwork[PairwiseDependency,NaiveFactor]

        

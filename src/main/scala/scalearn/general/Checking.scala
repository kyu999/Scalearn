package scalearn.general
    
object Checking{
            
    def requireDouble(subject:Double,criteria:Double,description:String) = 
        
        require( 
            subject <= criteria + 0.00001 
                        && 
            subject >= criteria - 0.00001 ,
            
            description
        )

}
package scalearn.preprocessing.cleansing
    
import scala.collection.mutable.ListBuffer
    
object TextFormator{

    
    /* 
    schema : country , year
    found  : "China, Makao" , 2001
             'Japan, Osaka' , 1993
    
    wanna convert => ListBuffer(China Makao , 2001)
    
    */
    
    def removeQuote(line: String): ListBuffer[String] = {
    
        val words = ListBuffer.empty[String]
            
        var word = ""
            
        var quoteEver = 's'
            
        val isQuote = 
            { subject: Char => subject == '"' || subject == ''' }
            
        line.foreach{ letter => 
            
            //すでにquoteが見つかっている
            if( isQuote(quoteEver) ){
                
                if(quoteEver == letter)
                    quoteEver = 's'  //quote 初期化
                    
                else if (letter == ',' || isQuote(letter) ){}
            
                else word = word + letter
                
               
            }else{ //初めてquoteを見つける
                
                if( isQuote(letter) ) 
                    quoteEver = letter 
                   
                else if(letter == ','){ 
                    words += word 
                    word = ""
                }
            
                else word = word + letter
            
            }
                   
        }
               
        words += word
               
        words
               
    }

}
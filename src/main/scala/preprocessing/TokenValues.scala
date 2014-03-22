package preprocessing

import tokenfactory.JP
import org.atilika.kuromoji._

trait TokenValues {
		
	val values:Map[Token,Int]

}

trait TokenProbability extends TokenValues{

}

trait TokenVector extends TokenValues{

	
}
import org.scalatest.FunSuite
import io.read
class Read_test extends FunSuite{
	
	val file_pathes = Vector("resource/doc1.txt","resource/doc2.txt","resource/doc3.txt","resource/doc4.txt","resource/doc5.txt","resource/doc6.txt")
	
	var counter = 0
	
	test("rdd"){
	
		val vector_rdd = file_pathes.map( path => read.rdds(path,false) )
		//localで実行したらメモリ不足になったためcacheしない。
		
		vector_rdd.foreach{
			rdd => 
				counter += 1
				rdd.saveAsTextFile("resource/num"+counter+".txt")}

	}
}
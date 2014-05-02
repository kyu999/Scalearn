package scalearn.statistics

import breeze.linalg._
import breeze.numerics._

object BreezeTest extends App{
    val vector = DenseVector(1,2,3,4)
    println("norm : "+norm(vector))
    println("vector : "+vector)
    
/*    val matrix = DenseMatrix( (1,2,3,4),(5,6,7,8))
    println("matrix : ")
    println(matrix)
    println("add 1 to each elements of the second raw   : "+( matrix(::,1) :=100 ) )
    println("add 1 to each elements of whole matrix  : "+( matrix(::,*) +100 ) )
    println("add 1 to each elements of the first column   : "+( matrix(1,::) :=(-1000) ) )
    println("matrix : ")
    println(matrix)
  
  DenseMatrixがうまくいかん。
*/
}
    
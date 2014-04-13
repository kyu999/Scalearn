package scalearn.statistics

import breeze.linalg._
import breeze.numerics._

object BreezeTest extends App{
    val vector = DenseVector(1,2,3,4)
    println("norm : "+norm(vector))
    println("vector : "+vector)
}
    
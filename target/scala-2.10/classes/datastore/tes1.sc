package datastore

object tes1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val x=Stream(0.5,3,5,3,3,6,7,8,54,5,2)          //> x  : scala.collection.immutable.Stream[Double] = Stream(0.5, ?)
  val y=Stream(5.0,3,5,3,3,6,7,8,54,5,2)          //> y  : scala.collection.immutable.Stream[Double] = Stream(5.0, ?)
  val data=new Dataset(x,y)                       //> data  : datastore.Dataset = datastore.Dataset@1abcd898
}
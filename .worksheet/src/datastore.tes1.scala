package datastore

object tes1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(76); 
  println("Welcome to the Scala worksheet");$skip(41); 
  val x=Stream(0.5,3,5,3,3,6,7,8,54,5,2);System.out.println("""x  : scala.collection.immutable.Stream[Double] = """ + $show(x ));$skip(41); 
  val y=Stream(5.0,3,5,3,3,6,7,8,54,5,2);System.out.println("""y  : scala.collection.immutable.Stream[Double] = """ + $show(y ));$skip(28); 
  val data=new Dataset(x,y);System.out.println("""data  : datastore.Dataset = """ + $show(data ))}
}

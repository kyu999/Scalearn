package io

import datafactory._
import Converter._
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession


object Raw extends Table[(Double)]("Raw"){
  def value = column[Double]("Value")
  def * = value			//*のあとは位置文字分あけるべし
}
/*
 *   def name = column[String]("Name",O.PrimaryKey)
object Data extends Table[(Double)]("Data"){
}
* 
*/

object Entity extends App{
  
	val sample=Vector(4,3,5,3,6,8,9,78,4,32,1,21)
	val data=sample.toda
	
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
	// Create the tables, including primary and foreign keys
    (Raw.ddl).create
    
//    sample.foreach(a=>Raw.insert(a))
    val resi=data.resi
    resi.raw.foreach(a=>Raw.insert(a))
    Raw.insert(data.regline(10))
    
    val raw=Query(Raw)
    
    raw.foreach(a=>println(a))
    
    }
}


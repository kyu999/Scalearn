package io
    
import scala.slick.driver.H2Driver.simple._

class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
    
  def id: Column[Int] = column[Int]("SUP_ID", O.PrimaryKey) 
  def name: Column[String] = column[String]("SUP_NAME")
  def street: Column[String] = column[String]("STREET")
  def city: Column[String] = column[String]("CITY")
  def state: Column[String] = column[String]("STATE")
  def zip: Column[String] = column[String]("ZIP")
  
  def * = (id, name, street, city, state, zip)
  //たぶんSELECT * FROM ~ 、の *　の部分を定義していると思われる。
}

class Coffees(tag: Tag) extends Table[(String,Int)] ( tag, "COFFEES"){
 
    def name: Column[String] = column[String]("NAME")
    def amount: Column[Int] = column[Int]("AMOUNT")
        
    def * = (name, amount)
}
    
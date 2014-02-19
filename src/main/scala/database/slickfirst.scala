import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession

//Tableの定義はSession内じゃなくてもいい。

// SUPPLIERSテーブルの定義
object Suppliers extends Table[(Int, String, String, String, 		String, String)]("SUPPLIERS") {
  def id = column[Int]("SUP_ID", O.PrimaryKey) // 主キー
  def name = column[String]("SUP_NAME")
  def street = column[String]("STREET")
  def city = column[String]("CITY")
  def state = column[String]("STATE")
  def zip = column[String]("ZIP")
  // 全てのテーブルではテーブルの型パラメタと同じタイプの射影*を定義する必要がある．
  def * = id ~ name ~ street ~ city ~ state ~ zip
}

// COFFEESテーブルの定義
object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
  def name = column[String]("COF_NAME", O.PrimaryKey)
  def supID = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  def sales = column[Int]("SALES")
  def total = column[Int]("TOTAL")
  def * = name ~ supID ~ price ~ sales ~ total
  // 他のテーブルとの結合のため作成された関係を表す外部キー
  def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
}

class slickfirst{
//Session scopeから外れているからinsert等は使えない 
//  def goin(content:String)=Coffees.insert((content,         101, 7.99, 0, 0))
    
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
	// Create the tables, including primary and foreign keys
	(Suppliers.ddl ++ Coffees.ddl).create
    
	// Insert some suppliers
	Suppliers.insert(101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199")
	Suppliers.insert( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460")
	Suppliers.insert(150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966")
   
	// Insert some coffees (using JDBC's batch insert feature, if supported by the DB)
	Coffees.insertAll(
			("Colombian",         101, 7.99, 0, 0),
			("French_Roast",       49, 8.99, 0, 0),
			("Espresso",          150, 9.99, 0, 0),
			("Colombian_Decaf",   101, 8.99, 0, 0),
			("French_Roast_Decaf", 49, 9.99, 0, 0)
			)

// Iterate through all coffees and output them
Query(Coffees) foreach { case (name, supID, price, sales, total) =>
  println("  " + name + "\t" + supID + "\t" + price + "\t" + sales + "\t" + total)
}

println(" id , name , street , city , state , zip")
Query(Suppliers) foreach{case (id,name,street,city,state,zip)=>
  println(" "+id+"\t"+name+"\t"+street+"\t"+city+"\t"+state+"\t"+zip)
}


	}
}
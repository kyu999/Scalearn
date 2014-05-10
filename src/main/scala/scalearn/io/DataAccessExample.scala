package io
    
//import scala.slick.driver.SQLiteDriver.simple._
import scala.slick.driver.H2Driver.simple._


object DataAccess extends App{
      
    val suppliers: TableQuery[Suppliers] = TableQuery[Suppliers]
    val coffees: TableQuery[Coffees] = TableQuery[Coffees]
        
//"jdbc:sqlite:/Users/Tsubaki/Document/Database/sample.db", driver = "org.sqlite.jDBC"
        
//"jdbc:h2:mem:hello" <- memを抜いたらdbファイルが作成される
  Database.forURL("jdbc:h2:/Users/Tsubaki/Documents/Scala/Scalearn/resource/database/hello", driver = "org.h2.Driver").withSession { implicit session =>
        //suppliers.ddl ++ 
//        (suppliers.ddl ++ coffees.ddl).create
        
/*        suppliers += (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199")
        suppliers += (49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460")
        suppliers += (150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
*/     
//        coffees += ("carot",45600)
        
        suppliers.foreach { case (id, name, street, city, state, zip) =>
            println(
            "  " + id + "\t" + name + "\t" + street 
            + "\t" + city + "\t" + state + "\t" + zip)
                          }
            
        println("-------------------------------")
            
        coffees.foreach{ case (name,amount) =>
            println("The amount of " + name + " is " + amount)
                       }
                                                                                    
    }
    

}

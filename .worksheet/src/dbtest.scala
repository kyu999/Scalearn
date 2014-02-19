object dbtest {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(59); 
  println("Welcome to the Scala worksheet");$skip(24); 
  val db=new slickfirst;System.out.println("""db  : slickfirst = """ + $show(db ));$skip(18); val res$0 = 
  db.goin("gogo");System.out.println("""res0: Int = """ + $show(res$0))}
  
  
}

#Cheat Sheet for Scalearn

1. how to deal with data statistically
2. special data: time series analysis
3. simple machine learning
4. data visualization


1. deal with data statistically

  If you are interested in statistical process like R does, you should look over here at least. First of all, what you have to do is to import necessary entity; in this case, "import datafactory._" After importing it, please look at "da.class" in datafactory  since that's the core class which is wrapper of collection in fact and it holds a bunch of statistical method, such as "mean","sd","reg"(stands for regression), and so on. To create the inscance of it, you have 3 alternatives; one is "new da( Seq[Double] )", the second is "da(...)", and the last one is "Seq(Double).toda". For example, if you have a list of number, 
  
  "val l=List(1,2,3,4,5)"
  
  1. "new da(l)"
  2. "da(1,2,3,4,5)"
  3. "l.toda"
  
  
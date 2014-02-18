package datastore

object UnitTes extends App{
	val x=Stream(0.5,3,5,3,3,6,7,8,5,2) 
	val y=Stream(5.0,3,5,3,3,6,7,8,5,2) 
	val data=new Dataset(x,y)
	data.summary
}
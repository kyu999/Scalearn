package datafactory

import math._

object Converter{
  
	implicit def DoubleVectortoData(in:Vector[Double]):Convertda=new Convertda(in)
	
	implicit def IntVectortoData(in:Vector[Int]):Convertda=new Convertda(in.map(a=>a.toDouble))
	
	
	implicit def toDataset(in:Vector[Vector[Double]]):Convertds=new Convertds(in)
	
	implicit def tomat(in:Vector[Vector[Double]]):ConvertMatrix=new ConvertMatrix(in)
	
	implicit def **(in:Int):ScaleCulMat=new ScaleCulMat(in)
	
}
class Convertda(in:Vector[Double]){
  def toda=da(in)
}
class Convertds(in:Vector[Vector[Double]]){
  def tods=new ds(in.map(a=>da(a)))
}

class ConvertMatrix(in:Vector[Vector[Double]]){
  def tomat=new m(in)
}

class ScaleCulMat(in:Int){
  def **(matrix:m)=matrix**(in)
}

/*暗黙の型変換用オブジェ＆クラス
  Vectorに本来ないメソッドtoda,todsが呼び出される
  ->暗黙の型変換
  ->Converter objectのtodaメソッドが呼び出される
  ->Convertdaインスタンス作成
  ->Convertdaのtodaメソッドが呼び出される
  ※使用するにはConverterオブジェクトをimportするだけ。ぶっちゃけコンパニオンオブジェクトを使用して
  newなしでda,dsインスタンス作成できるからあとは好みの問題。
  ちなみにConvertdaseクラスのtodsメソッドでdsのインスタンス作成にnewを使っているのは引数が可変長引数の関係で
  マッチしないから直接dsクラスを作成している
  */

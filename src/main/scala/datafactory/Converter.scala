package datafactory

import math._

object Converter {
	implicit def toda(in:Vector[Double]):ConvertData=new ConvertData(in)
	implicit def tods(in:Vector[Vector[Double]]):ConvertDataset=new ConvertDataset(in)
	implicit def tomat(in:Vector[Vector[Double]]):ConvertMatrix=new ConvertMatrix(in)
	
}
class ConvertData(in:Vector[Double]){
  def toda=data(in)
}
class ConvertDataset(in:Vector[Vector[Double]]){
  def tods=new dataset(in.map(a=>data(a)))
}
class ConvertMatrix(in:Vector[Vector[Double]]){
  def tomat=new m(in)
}

/*暗黙の型変換用オブジェ＆クラス
  Vectorに本来ないメソッドtoda,todsが呼び出される
  ->暗黙の型変換
  ->Converter objectのtodaメソッドが呼び出される
  ->ConvertDataインスタンス作成
  ->ConvertDataのtodaメソッドが呼び出される
  ※使用するにはConverterオブジェクトをimportするだけ。ぶっちゃけコンパニオンオブジェクトを使用して
  newなしでdata,datasetインスタンス作成できるからあとは好みの問題。
  ちなみにConvertDataseクラスのtodsメソッドでdatasetのインスタンス作成にnewを使っているのは引数が可変長引数の関係で
  マッチしないから直接datasetクラスを作成している
  */

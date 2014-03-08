package datafactory

import math._

object Converter{
  
	implicit def DoubleSeqtoData(in:Seq[Double]):Convertda=
	  in match{
	  case v:Vector[Double]=>new Convertda(v)
	  case _=>new Convertda(in.toVector)
	}
	implicit def IntSeqtoData(in:Seq[Int]):Convertda=
	  in match{
	  case v:Vector[Int]=>new Convertda(v.map(a=>a.toDouble))
	  case _=>new Convertda(in.toVector.map(a=>a.toDouble))
	}
	
	implicit def toDataset(in:Seq[Seq[Double]]):Convertds=
	  in match{
	  case vs:Vector[Seq[Double]]=>new Convertds(vs.map(a=>a.toVector))
	  case ss:Seq[Seq[Double]]=>new Convertds(in.toVector.map(a=>a.toVector))
	}
	
	implicit def tomat(in:Seq[Seq[Double]]):ConvertMatrix=
	  in match{
	  case vs:Vector[Seq[Double]]=>new ConvertMatrix(vs.map(a=>a.toVector))
	  case ss:Seq[Seq[Double]]=>new ConvertMatrix(ss.toVector.map(a=>a.toVector))
	}
	
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

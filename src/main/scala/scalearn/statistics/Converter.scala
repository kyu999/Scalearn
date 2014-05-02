package scalearn.statistics

import scalearn.math._

object Converter{
    implicit def VectorDoubleToData(in:Vector[Double]):ToData = new ToData(in)
	
    implicit def VectorIntToData(in:Vector[Int]):ToData = new ToData(in.map(a=>a.toDouble))
        
    implicit def tods(in:Vector[Vector[Double]]):ToDase=new ToDase(in)
        
    implicit def toinf(in:Vector[Vector[Double]]):ToInfer=new ToInfer(in)

    implicit def tomat(in:Vector[Vector[Double]]):ToMatrix=new ToMatrix(in)
	
    implicit def **(in:Int):ScaleCulMat=new ScaleCulMat(in)
	
}

class ToData(in:Vector[Double]){
    def toda=data(in)
}

class ToDase(in:Vector[Vector[Double]]){
    def tods=new dase(in.map(a=>data(a)))
}

class ToInfer(in:Vector[Vector[Double]]){
    def toinf=new infds(in.map(a=>data(a)))
}

class ToMatrix(in:Vector[Vector[Double]]){
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

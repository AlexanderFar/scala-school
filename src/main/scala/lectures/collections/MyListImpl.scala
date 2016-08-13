package lectures.collections

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Представим, что по какой-то причине, Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, что бы выполнить задание,
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList(data: List[Int]) {

    def flatMap(f: (Int => MyList)) = MyList(data.flatMap(inp => f(inp).data))

    def map(f: Int => Int): MyList = this.flatMap(i => MyList(List(f(i))))

    @tailrec final def foldLeft(acc: Int)(f: Tuple2[Int, Int] => Int): Int =
      if (data.isEmpty)
        acc
      else
        MyList(data.tail).foldLeft(f(acc, data.head))(f)


    //    Вопрос: не придумал как сделать filter через map, не используя других функций и не меняя его(map) сигнатуры
    def filter(p: Int => Boolean) = this.flatMap(i => {
      if (p(i))
        MyList(List(i))
      else
        MyList(List())
    })
  }

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList(Nil).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

  case class MyGList[T, TList <: GenTraversable[T]](data: TList) {

    def flatMap[UList <: GenTraversable[T]](f: T => MyGList[T, UList]) = MyGList[T, GenTraversable[T]](data.flatMap(x => f(x).data))

    def map(f: T => T) = this.flatMap(i => MyGList[T, GenTraversable[T]](GenTraversable(f(i))))

    def foldLeft(acc: T)(f: Tuple2[T, T] => T): T =
      if (data.isEmpty)
        acc
      else
        MyGList[T, GenTraversable[T]](data.tail).foldLeft(f(acc, data.head))(f)

    def filter(p: T => Boolean) = this.flatMap(i => {
      if (p(i))
        MyGList(GenTraversable[T](i))
      else
        MyGList(GenTraversable[T]())
    })
  }

  require(MyGList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).flatMap(i => MyGList[Int, List[Int]](List(i, 10 + i))).data == List(1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16))
  require(MyGList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyGList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyGList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyGList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyGList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

}
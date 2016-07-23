package lectures.collections

import scala.annotation.tailrec

/**
  * Представим, что по какой-то причине, Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap
  * * * * * filter(???) - через метод map
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
}
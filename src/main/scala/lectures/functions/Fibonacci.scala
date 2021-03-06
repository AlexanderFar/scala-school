package lectures.functions

import scala.annotation.tailrec

/**
  * Цель упражнения, вычислить 9 - е число Фибоначчи
  * Для этого, раскомментируйте строчку в методе fibs и исправьте ошибку компиляции.
  *
  * Данная реализация вычисления чисел фибоначчи крайне не оптимальна (имеет показатеьную сложность O(a.pow(n)) )
  * Для того, что бы в этом убедиться Вы можете раскомментировать
  * строчку с вычислением 1000-ого числа фибоначчи
  *
  */
object Fibonacci extends App {

  // Task 2
  def fibs(num: Int): Int = {
    if (num == 1) 1
    else if (num == 2) 1
    else fibs(num - 1) + fibs(num - 2)
  }

  println(fibs(9))
  //println(fibs(1000))
}

/**
  * Цель упражнения, используя приемы динамического программирования
  * реаилзовать более оптимальный алгоритм подсчета чисел фибоначчи
  * Для этого нужно реализовать функцию fibsImpl.
  * Сигнатуру функции Вы можете расширять по своему усмотрению,
  * но реализация должна удовлетварять следующим требованиям
  * * * * метод fibsImpl - должен быть tail recursive
  * * * * параметр acc - аккумулятор посчитанных значений
  *
  */
object Fibonacci2 extends App {

  private val startAcc = Array(1, 1, 2, 3, 5, 8, 13)
  //    private val startAcc = Array(1, 1, 2)
  private val startAccLength = startAcc.length

  def fibs2(num: Int) =
    if (num <= startAccLength)
      startAcc(num - 1)
    else
      fibsImpl(num, startAcc)(num - 1)

  @tailrec private def fibsImpl(num: Int, acc: Array[Int]): Array[Int] = {
    if (num <= startAccLength)
      acc
    else {
      val l = acc.length;
      fibsImpl(num - 1, acc :+ (acc(l - 1) + acc(l - 2)))
    }
  }

  println(fibs2(9))
  println(fibs2(1000))
}





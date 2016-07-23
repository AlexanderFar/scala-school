package lectures.collections

import scala.annotation.tailrec

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    if (data == null || data.length <= 1)
      data
    else {
      val partitions = data.splitAt(data.length / 2)
      val left = mergeSort(partitions._1)
      val right = mergeSort(partitions._2)

      merge(Seq[Int](), left, right)
    }
  }

  @tailrec private def merge(acc: Seq[Int], left: Seq[Int], right: Seq[Int]): Seq[Int] = {
    if (left.nonEmpty && right.nonEmpty) {
      if (left.head <= right.head)
        merge(acc :+ left.head, left.tail, right)
      else
        merge(acc :+ right.head, left, right.tail)
    }
    else if (left.nonEmpty)
      acc ++ left
    else if (right.nonEmpty)
      acc ++ right
    else
      acc
  }

  require(mergeSort(Seq(1)) == List(1))
  require(mergeSort(Seq()) == List())
  require(mergeSort(Seq(2, 1)) == List(1, 2))
  require(mergeSort(Seq(1, 2)) == List(1, 2))
  require(mergeSort(Seq(3, 2, 4, 5, 6, 7, 1, 2, 2, 8)) == List(1, 2, 2, 2, 3, 4, 5, 6, 7, 8))

}

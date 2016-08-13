package lectures.oop.types

import lectures.matching.SortingStuff.Watches
import scala.annotation.tailrec

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class реализуте GeneralBSTImpl таким образом,
  * что бы дерево могло работать с произвольным типом данных
  *
  * Наслеников GeneralBSTImpl определять нельзя
  *
  * Создайте генератор для деревьев 3-х типов данных
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаються часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}

case class GeneralBSTImpl[T: Ordering](value: T,
                                       left: Option[GeneralBSTImpl[T]] = None,
                                       right: Option[GeneralBSTImpl[T]] = None) extends GeneralBST[T] {


  def add(newValue: T): GeneralBST[T] = addInner(newValue)

  def addInner(newValue: T): GeneralBSTImpl[T] =
    if (implicitly[Ordering[T]].lt(newValue, value))
      GeneralBSTImpl(value, left.map(x => x.addInner(newValue)) orElse Option(GeneralBSTImpl(newValue)), right)
    else if (implicitly[Ordering[T]].gt(newValue, value))
      GeneralBSTImpl(value, left, right.map(x => x.addInner(newValue)) orElse Option(GeneralBSTImpl(newValue)))
    else
      this

  def find(value: T): Option[GeneralBST[T]] = findInner(value)

  def findInner(value: T): Option[GeneralBST[T]] =
    if (implicitly[Ordering[T]].equiv(this.value, value))
      Option(this)
    else if (implicitly[Ordering[T]].lt(this.value, value))
      this.right.flatMap(x => x.findInner(value))
    else
      this.left.flatMap(x => x.findInner(value))
}

trait Randomizer[T] {
  def rnd(): T
}

class GeneralBSTBuilder[T: Ordering] {

  @tailrec final def genTree(count: Int, node: GeneralBST[T])(implicit rnd: Randomizer[T]): GeneralBST[T] =
    if (count == 0)
      node
    else
      genTree(count - 1, node.add(rnd.rnd()))

  def build(count: Int)(implicit rnd: Randomizer[T]) = genTree(count, GeneralBSTImpl[T](rnd.rnd()))

}


object GeneralBSTBuilder extends App {
  implicit val WatchesOrdering: Ordering[Watches] = new Ordering[Watches]() {
    def compare(a: Watches, b: Watches) = a.cost compare b.cost
  }

  implicit val floatRnd: Randomizer[Float] = new Randomizer[Float] {
    override def rnd(): Float = (Math.random() * 1000).toFloat
  }
  implicit val stringRnd: Randomizer[String] = new Randomizer[String] {
    override def rnd(): String = scala.util.Random.alphanumeric.take(10).mkString
  }
  implicit val watchesRnd: Randomizer[Watches] = new Randomizer[Watches] {
    override def rnd(): Watches = Watches(stringRnd.rnd(), floatRnd.rnd())
  }

  var floatTree = new GeneralBSTBuilder[Float]().build(5)
  var stringTree = new GeneralBSTBuilder[String]().build(5)
  val watchesTree = new GeneralBSTBuilder[Watches]().build(5)
}



package lectures.oop

import scala.annotation.tailrec


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения(null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl.
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в  условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужгно раскомментировать и реадизовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def breadthTraverseSearch(value: Int): Option[BST]

  //т.к. нет договорённости о порядке обхода дерева, f должна быть ассоциативна
  def fold(aggregator: Int)(f: (Int, Int) => (Int)):Int
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {


  def add(newValue: Int): BST = addInner(newValue)

  def addInner(newValue: Int): BSTImpl =
    if (newValue < value)
      BSTImpl(value, left.map(x => x.addInner(newValue)) orElse Option(BSTImpl(newValue)), right)
    else if (value < newValue)
      BSTImpl(value, left, right.map(x => x.addInner(newValue)) orElse Option(BSTImpl(newValue)))
    else
      this


  def find(value: Int): Option[BST] = findInner(value)

  def findInner(value: Int): Option[BSTImpl] =
    if (this.value == value)
      Option(this)
    else if (this.value < value)
      this.right.flatMap(x => x.findInner(value))
    else
      this.left.flatMap(x => x.findInner(value))


  def breadthTraverseSearch(value: Int): Option[BST] = breadthTraverseSearchBase(x => x.value == value)

  def breadthTraverseSearchBase(predicate: BST => Boolean): Option[BST] = breadthTraverseProcces(List(Option(this)), predicate).headOption.getOrElse(None)

  @tailrec private def breadthTraverseProcces(list: List[Option[BST]], predicate: BST => Boolean): List[Option[BST]] = {
    if (list.isEmpty)
      list
    else {
      var target: Option[BST] = None
      val nextLevel = list.withFilter(_ => target.isEmpty)
        .map(node =>
          node.map(x => {
            if (predicate(x))
              target = Option(x)
            List(x.left, x.right)
          })
        )
      if (target.nonEmpty)
        List(target)
      else
        breadthTraverseProcces(nextLevel.flatten.flatten, predicate)
    }
  }

  //т.к. нет договорённости о порядке обхода дерева, f должна быть ассоциативна
  def fold(aggregator: Int)(f: (Int, Int) => (Int)) = foldInner(this, aggregator, f)

  def foldInner(node: BSTImpl, acc: Int, f: (Int, Int) => Int): Int = {
    val left = node.left.map(x => foldInner(x, acc, f)).getOrElse(acc)
    val right = node.right.map(x => foldInner(x, left, f)).getOrElse(left)
    f(right, node.value)
  }


  override def toString() = printLineItems(getPrintItems(Option(this), 1, 0))

  //служебный класс для алгоритма печати дерева
  private case class printItem(value: Int, level: Int, width: Int, shift: Int) {
  }

  //подготавливаем данные для печати дерева
  private def getPrintItems(root: Option[BSTImpl], level: Int, shift: Int): List[printItem] =
    root.map(x => {
      val leftPrintItems = getPrintItems(x.left, level + 1, shift)
      val leftWidth = leftPrintItems.headOption.map(y => y.width).getOrElse(0)
      val rightPrintItems = getPrintItems(x.right, level + 1, shift + leftWidth + getNumWidth(x.value))
      val width = leftWidth + rightPrintItems.headOption.map(y => y.width).getOrElse(0) + getNumWidth(x.value)
      val e = new printItem(x.value, level, width, shift + leftWidth)
      e :: (leftPrintItems ++ rightPrintItems)
    }).getOrElse(List[printItem]())

  //печать дерева поуровнева
  private def printLineItems(list: List[printItem]): String =
    if (list.isEmpty)
      "list is empty\n"
    else
      (1 to list.maxBy(x => x.level).level)
        .map(l =>
          printOneLevelItems("", list.filter(x => x.level == l).sortBy(x => x.shift), 0)
        ).foldLeft("")((a, s) => a + s)

  //печать отдельного уровня
  @tailrec private def printOneLevelItems(acc: String, list: List[printItem], skip: Int): String =
    if (list.isEmpty) {
      acc + "\n"
    } else {
      val placeholderCount = list.head.shift - skip
      val str = acc + " " * placeholderCount + list.head.value
      printOneLevelItems(str, list.tail, skip + placeholderCount + getNumWidth(list.head.value))
    }

  //сколько символов занимает число
  private def getNumWidth(v: Int): Int = v.toString.length
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  if (nodesCount <= 0)
    throw new IllegalArgumentException("count must be greater than zero")

  def rnd(): Int = (Math.random() * maxValue).toInt

  val markerItem = rnd()
  val markerItem2 = rnd()
  val markerItem3 = rnd()

  //генератор узлов
  //добавляет в существующие дерево сount узлов
  @tailrec def genTree(count: Int, node: BST): BST =
    if (count == 0)
      node
    else
      genTree(count - 1, node.add(rnd()))

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = genTree(nodesCount, root)

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)

  require(testTree.breadthTraverseSearch(markerItem).isDefined)
  require(testTree.breadthTraverseSearch(markerItem2).isDefined)
  require(testTree.breadthTraverseSearch(markerItem3).isDefined)

  println(testTree)

  /*
    пример из презентации
    var t = BSTImpl(100).add(15).add(3).add(13).add(91).add(17).add(190).add(171).add(155).add(205).add(303)
    println(t)

           100
     15             190
  3      91      171   205
   13  17     155         303
  */

  var t = BSTImpl(3).add(2).add(4)
  require(t.fold(0)((a, b) => a + b) == 9)
  require(t.fold(0)((a, b) => a * b) == 0)
  require(t.fold(1)((a, b) => a * b) == 24)
  require(t.fold(10)((a, b) => a + b) == 19)
  require(t.fold(10)((a, b) => a * b) == 240)

}
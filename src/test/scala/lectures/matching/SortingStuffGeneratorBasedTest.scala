package lectures.matching

import lectures.matching.SortingStuff.{Book, Stuff, StuffBox, Watches, Boots}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то  метод findMyKnife его отыщет
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))

  /*
    пришлось заменить реализацю interestingBookGen, потому что тесты падали с ошибкой
    Gave up after 12 successful property evaluations. 89 evaluations were discarded.
      ScalaTestFailureLocation: org.scalatest.prop.GeneratorDrivenPropertyChecks$class at (GeneratorDrivenPropertyChecks.scala:914)
    org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException: Gave up after 12 successful property evaluations. 89 evaluations were discarded.
      at org.scalatest.prop.Checkers$.doCheck(Checkers.scala:387)
    at org.scalatest.prop.GeneratorDrivenPropertyChecks$class.forAll(GeneratorDrivenPropertyChecks.scala:914)
    at lectures.matching.SortingStuffGeneratorBasedTest.forAll(SortingStuffGeneratorBasedTest.scala:32)
    at lectures.matching.SortingStuffGeneratorBasedTest$$anonfun$2$$anonfun$apply$mcV$sp$3$$anonfun$apply$mcV$sp$4.apply$mcV$sp(SortingStuffGeneratorBasedTest.scala:70)
  */

  val interestingBookGen = Gen.alphaStr.map(name => Book(name, true))
  //  val interestingBookGen = bookGenerator.filter(_.isInteresting)

  val knifeGen: Gen[Option[Stuff]] = Gen.option(SortingStuff.Knife)

  val adidasBootGenerator = Gen.zip(Gen.choose(15, 60)).map(size => Boots("Adidas", size))
  val converseBootGenerator = Gen.zip(Gen.choose(15, 60)).map(size => Boots("Converse", size))
  val otherBootGenerator = Gen.zip(Gen.alphaStr, Gen.choose(15, 60)).filter(x => x._1 != "Adidas" && x._1 != "Converse").map(b => Boots(b._1, b._2))

  // Override configuration if you need
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val booksGenerator = Gen.listOf(interestingBookGen)
        val watchesGenerator = Gen.listOf(cheepWatchGen)
        forAll(Gen.zip(booksGenerator, watchesGenerator)) { tpl =>
          val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(tpl._1 ++ tpl._2).toList)

          goodBooks should have size tpl._1.size
          niceWatches should have size 0
          junk should have size tpl._2.size
        }
      }
    }

    "find knife" which {
      "was occasionally disposed" in {
        forAll(Gen.zip(knifeGen, bookGenerator, interestingBookGen, cheepWatchGen)) { tpl =>
          val box = SortingStuff.sortJunk(Random.shuffle(List(tpl._2, tpl._3, tpl._4) ++ tpl._1).toList)
          SortingStuff.findMyKnife(box) shouldBe tpl._1.nonEmpty
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        val adidasBootGen = Gen.listOf(adidasBootGenerator)
        val converseBootGen = Gen.listOf(converseBootGenerator)
        val otherBootGen = Gen.listOf(otherBootGenerator)

        forAll(Gen.zip(adidasBootGen, converseBootGen, otherBootGen)) { tpl =>
          val StuffBox(_, _, boots, junk) = SortingStuff.sortJunk(Random.shuffle(tpl._1 ++ tpl._2 ++ tpl._3).toList)

          boots should have size tpl._1.size + tpl._2.size
          junk should have size tpl._3.size
        }
      }
    }
  }
}


package lectures.functions

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

/**
  * Авторизация это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with Matchers with PropertyChecks {
  //  val anonymousUserGenerator =
  val testUserGenerator: Gen[User] = Gen.oneOf[User](AnonymousUser(), CardUser(), LPUser())
  val wrongUserGenerator = testUserGenerator.filter(u => u match {
    case it: CardUser => !AuthenticationData.registeredCards.contains(it.credentials)
    case it: LPUser => !AuthenticationData.registeredLoginAndPassword.contains(it.credentials)
    case _ => true
  })

  val goodLPUserGenerator = Gen.zip(Gen.chooseNum(1, 9999), Gen.oneOf(AuthenticationData.registeredLoginAndPassword.toList)).map(x => new LPUser(x._1, x._2))
  val goodCardUserGenerator = Gen.zip(Gen.chooseNum(1, 9999), Gen.oneOf(AuthenticationData.registeredCards.toList)).map(x => new CardUser(x._1, x._2))
  val goodUserGenerator: Gen[User] = Gen.oneOf(goodLPUserGenerator, goodCardUserGenerator)
  // Override configuration if you need
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10, maxSize = 20)

  "Authentication" should {

    "authByCard" which {
      "must be define for user registered by cards" in {
        forAll(goodCardUserGenerator) { user =>
          Authentication.authByCard.isDefinedAt(user) shouldBe true
        }
      }
      "must be not define for user not registered by cards" in {
        forAll(wrongUserGenerator) { user =>
          Authentication.authByCard.isDefinedAt(user) shouldBe false
        }
      }
    }


    "authByLP" which {
      "must be define for user registered by password" in {
        forAll(goodLPUserGenerator) { user =>
          Authentication.authByLP.isDefinedAt(user) shouldBe true
        }
      }
      "must be not define for user not registered by password" in {
        forAll(wrongUserGenerator) { user =>
          Authentication.authByLP.isDefinedAt(user) shouldBe false
        }
      }
    }


    "authenticated" which {
      "only good users" in {
        forAll(Gen.zip(Gen.listOf(goodUserGenerator), Gen.listOf(wrongUserGenerator))) { tpl =>
          Authentication.authenticated(tpl._1 ++ tpl._2) should have size tpl._1.size
        }
      }
    }
  }
}




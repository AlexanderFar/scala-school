package lectures.collections

import scala.util.Random

/**
  * В этот задании Вам предстоит работать с очень неестабильным внешним сервисом.
  *
  * Для успешного завершения задания, вы должны реализовать метод buisnessLogic в объекте OptionVsNPE
  * Этот метод должен делать следующее
  * * * * * Получить и распечатать результат или, если была ошибка ResourceException,
  * распечатать "Try again with new resource" и повторить все заново
  * * * * * Получить ресурс через ResourceProducer
  * * * * * Если ресурс не получен, кидать ResourceException (throw new ResourceException)
  * * * * * Если ресурс удачно получен, на его основе получить Connection
  * * * * * Если соединение не получено, пробовать, пока соединение не будет получено
  * * * * * Вызвать у соединения метод result()
  * * * * * Если метод result возвращает Null, заменить его дефолтным сообщением из ОБъекта Connection
  *
  * Для успешного решения задания
  * * * * * Замените знаки вопроса реализацией методов
  * * * * * Нельзя менять, содержимое объектов ConnectionProducer, ResourceProducer
  * * * * * Не должно быть ни одного NPE
  * * * * * Можно менять входные и выходные параметры методов Connection
  *
  * трейт FailUtil - содержит методы для эмуляции спародических ошибок
  *
  *
  */
class ResourceException extends Exception("Ресурс не отвечает")

trait FailUtil {
  val failRate: Double

  def timeToFail = Math.random() > failRate
}

object ResourceProducer extends FailUtil {
  def produce = if (timeToFail) null else Resource(Random.alphanumeric.take(10).mkString)

  val failRate: Double = 0.3
}

object ConnectionProducer extends FailUtil {
  val failRate: Double = 0.5

  def produce(resource: Resource) = if (timeToFail) null else Connection(resource)

  def result(connection: Connection) = if (timeToFail) null else connection.resource.name
}

case class Connection(resource: Resource) {
  private val defaultResult = "something went wrong!"

  def result(): String = {
    Option(ConnectionProducer.result(this)) getOrElse defaultResult
  }
}

case class Resource(name: String)

object OptionVsNPE extends App {

  def businessLogic(maxRetryResourceProduceCount: Int, maxRetryConnectProduceCount: Int): Option[String] = {
    val result =
      tryGetValue(maxRetryResourceProduceCount, GetResourceWithCatchException _)
        .flatMap(r =>
          tryGetValue(maxRetryConnectProduceCount, () => Option(ConnectionProducer.produce(r)))
            .map(c => c.result())
        )

    println(result)
    result
  }

  for (i <- 1 to 1000) {
    businessLogic(10, 100)
  }

  //  http://stackoverflow.com/questions/2742719/how-do-i-break-out-of-a-loop-in-scala
  //  Без break совсем без переменной не получилось
  private def tryGetValue[T](maxRetryCount: Int, f: () => Option[T]): Option[T] = {
    var result: Option[T] = None

    (1 to maxRetryCount).toStream
      .takeWhile(_ => result.isEmpty)
      .foreach(_ => result = f())

    result
  }

  private def GetResourceWithCatchException: Option[Resource] = {
    try {
      Option(ResourceProducer.produce) match {
        case r: Some[Resource] => r
        case _ => throw new ResourceException
      }
    } catch {
      case e:
        ResourceException => println("Try again with new resource")
        None
    }
  }


}

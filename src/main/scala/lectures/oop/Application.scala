package lectures.oop

import lectures.functions.SQLAPI

/**
  * У вас есть приложение, которое можно запустить в тестовом или продуктовом окружении
  * В зависимости от окружения ваше приложение создает БД с тестовым или бовым адресом
  * и использует подходящую реализацию сервиса.
  *
  * Ваша задача, реализовать методы doSomeService в тестовом и боевом сервисах. Для этого
  * * * * добавьте SQLAPI как self type annotation
  * * * * уберите знаки вопроса и раскомментируйте execute(sql)
  * * * * допишите тело функций в соответсвии с комментарием в каждом методе
  * После этого допишите инициализацию usefulService в Application, так что бы
  * в тестовой среде использовался TestServiceImpl
  * в боевой - ProductionServiceImpl
  *
  * Допишите тесты в ApplicationTest
  *
  */
trait UsefulService {
  def doSomeService(): Int
}

trait TestServiceImpl extends UsefulService {
  sqlApi: SQLAPI =>
  private val sql = "do the SQL query and then count words"

  def doSomeService() = (sqlApi.execute _ andThen doWork) (sql)

  // подсчитайте количество слов в результате execute
  def doWork(answer: String): Int = answer.split("""\s""").length
}

trait ProductionServiceImpl extends UsefulService {
  sqlApi: SQLAPI =>
  private val sql = "do the SQL query and than count 'a' sympols"

  def doSomeService(): Int = (sqlApi.execute _ andThen doWork) (sql)

  // подсчитайте сколько символов 'a' в полученной строке
  def doWork(answer: String): Int = answer.count(x => x == 'a')

}

class Application(isTestEnv: Boolean) {

  val usefulService: UsefulService = if (isTestEnv)
  //передайте "test db Resource" в качестве ресурсв в конструктор SQLAPI
    new SQLAPI("test db Resource") with TestServiceImpl
  else
  //передайте "production Resource" в качестве ресурсв в конструктор SQLAPI
    new SQLAPI("production Resource") with ProductionServiceImpl


  def doTheJob() = usefulService.doSomeService()
}

/*
object Application extends App {
  println(new Application(true).doTheJob)
  println(new Application(!true).doTheJob)
}
*/


package lectures.functions

/**
  * Представим себе, как бы мог выглядеть API для работы, например, с БД
  * Строить методы этого API будем через композицию уже определенных методов.
  *
  * Ваша задача реализовать метод execute, композируя методы из объекта SQLAPI
  * Метод execute из объекта SQLAPI должен выполнить следующие действия
  * * * * * залогировать ресурс
  * * * * * получить соединение с помощью метода connection
  * * * * * залогировать соединение
  * * * * * открыть соединение
  * * * * * выполнить SQL
  * * * * * залогировать результат
  *
  *
  * Обратите внимание на то, что композиция функций учит писать код в декларативном виде
  * Благодаря этому мы можем отделить реализацию методов от их применения и, в конечном итоге, иметь переиспользуемую
  * декларацию, которую можно применить, например, для настоящей БД
  *
  *
  */
class SQLAPI(resource: String) {

  case class Connection(resource: String, opened: Boolean = false) {

    private val result = "SQL has been executed. Congrats!"

    def open(): Connection = {
      println("open connection")
      this.copy(opened = true)
    }

    def execute(sql: String): String =
      if (opened)
        sql
      else
        throw new Exception("You have to open connection before execute")

  }

  private def logParamter[T](prm: T): T = {
    println(prm)
    prm
  }

  val connection = (resource: String) => Connection(resource)

  def execute(sql: String): String =
    ((logParamter[String] _ andThen connection andThen logParamter andThen openConnectionAndExecute) (resource) andThen logParamter) (sql)


  def openConnectionAndExecute(connection: Connection): (String) => String =
    (sql: String) => {
      connection.open execute sql
    }
}


object SQLCheck extends App {

  new SQLAPI("some DB").execute("some SQL")

}


package lectures.operators

import lectures.functions.{Computation, CurriedComputation, Data, FunctionalComputation}

/**
  * В задачке из lectures.functions.Computations, мы реализовали
  * один и тот же метод 3-я разными способами
  *
  * Пришло время оценить на сколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого, раскомментируйте код, выполните в циклах вызов 3-х имплементаций
  * Оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {


  var startTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 Computation.computation(
  for (i <- 1 to 100) {
    Computation.computation(filterData, dataArray)
  }
  println("elapsed time in Computation.computation - " + (System.currentTimeMillis() - startTimestamp))

  startTimestamp = System.currentTimeMillis()
  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction(
  for (i <- 1 to 100) {
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }
  var diff = System.currentTimeMillis() - startTimestamp
  println("elapsed time - " + diff)

  startTimestamp = System.currentTimeMillis()
  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 FunctionalComputation.filterApplied
  for (i <- 1 to 100) {
    FunctionalComputation.filterApplied(dataArray)
  }
  val time = System.currentTimeMillis() - startTimestamp
  println("elapsed time - " + time)

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  diff = diff - time

  print(s"Difference is about $diff milliseconds")
}
/*
результат

elapsed time in Computation.computation - 11243
elapsed time - 15105
elapsed time - 15089
Difference is about 16 milliseconds

1 не получилось запустить если CurriedComputation.partiallyAppliedCurriedFunction  и FunctionalComputation.filterApplied объявлены как val
  что нужно сделать что бы они были инициализированны

2 Насколько я плнимаю результат предпологался другой. Возможно в первом задачии не там расставил sleep. Что нужно поправить?
*/

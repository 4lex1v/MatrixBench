package com.alex

import collection.mutable.ArrayBuffer

object Main extends App {

  val runtime = Runtime.getRuntime

  type Matrix = Array[Array[Int]]

  implicit val grid: Array[Array[Int]] = Array(
    Array( 1, 2, 3,  4, 5, 6,  7, 8, 9),
    Array(11,12,13, 14,15,16, 17,18,19),
    Array(21,22,23, 24,25,26, 27,28,29),

    Array(31,32,33, 34,35,36, 37,38,39),
    Array(41,42,43, 44,45,46, 47,48,49),
    Array(51,52,53, 54,55,56, 57,58,59),

    Array(61,62,63, 64,65,66, 67,68,69),
    Array(71,72,73, 74,75,76, 77,78,79),
    Array(81,82,83, 84,85,86, 87,88,89)
  )

  implicit class AlgBench(alg: String) {
    def in(res: String) { println(alg + " in " + res) }
  }

  def alg1(grid: Matrix): Matrix =
    (for(x <- 0 until 9 by 3) yield
      (for {
        row <- 0 until 9;
        col <- x until x + 3 by 3;
        i <- col until col + 3
      } yield grid(row)(i)).toArray).toArray

  def alg2(grid: Matrix): Matrix = grid.map(_.grouped(3).toArray).grouped(3).toArray.map(_.transpose).flatten.map(_.flatten)

  def alg3(grid: Matrix): Matrix = grid.grouped(3).map(g => g.transpose.grouped(3).map(_.flatten.sorted)).flatten.toArray

  def alg4(grid: Matrix): Matrix = (0 until grid(0).size/3).toArray.map(i => grid.flatMap(a => a.drop(i*3).take(3)))

  def alg5(grid: Matrix): Matrix = Array.tabulate(9, 9){ (r, c) => grid(r/3*3+c/3)(r%3*3+c%3) }

  def time[R](func: => R) = {
    val start = System.nanoTime
    val freeMemo = runtime.freeMemory
    func
    val memo = runtime.freeMemory
    val end = System.nanoTime
    ((end - start), (freeMemo - memo))
  }

  def test(cycles: Int)(alg: Matrix => Matrix)(implicit grid: Matrix) = {
    val stats = for (run <- 1 to cycles) yield time(alg(grid))
    val res = stats.foldLeft((0L, 0L))((acc, cur) => ((acc._1 + cur._1)/2, (acc._2 + cur._2)/2))
    s"Cycles: $cycles Time: ${res._1}ns Memory: ${res._2}"
  }

  def b10     = test(10)_
  def b100    = test(100)_
  def b1000   = test(1000)_
  def b10000  = test(10000)_
  def b100000 = test(100000)_

  "alg1" in b10(alg1)
  "alg1" in b100(alg1)
  "alg1" in b1000(alg1)
  "alg1" in b10000(alg1)
  "alg1" in b100000(alg1)

  println

  "alg2" in b10(alg2)
  "alg2" in b100(alg2)
  "alg2" in b1000(alg2)
  "alg2" in b10000(alg2)
  "alg2" in b100000(alg2)

  println

  "alg3" in b10(alg3)
  "alg3" in b100(alg3)
  "alg3" in b1000(alg3)
  "alg3" in b10000(alg3)
  "alg3" in b100000(alg3)

  println

  "alg4" in b10(alg4)
  "alg4" in b100(alg4)
  "alg4" in b1000(alg4)
  "alg4" in b10000(alg4)
  "alg4" in b100000(alg4)

  println

  "alg5" in b10(alg5)
  "alg5" in b100(alg5)
  "alg5" in b1000(alg5)
  "alg5" in b10000(alg5)
  "alg5" in b100000(alg5)
}
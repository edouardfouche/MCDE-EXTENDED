package io.github.edouardfouche.index.tuple

case class CountTupleIndex(tuple: (Int, Array[Int])) extends TupleIndex {
  type T = (Int, Array[Int]) // For each category, we get a list of indexes
  val position = tuple._1
  val value = tuple._2
  val correction = 0
}

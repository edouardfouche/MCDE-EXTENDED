package io.github.edouardfouche.index.tuple

case class RankTupleIndex(tuple: Int) extends TupleIndex {
  type T = Int
  val position = tuple
  val value = tuple.toFloat
  val correction = tuple
  //val value = 0
}

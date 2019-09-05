package io.github.edouardfouche.index.tuple

case class AdjustedRankTupleIndex(tuple: (Int, Float)) extends TupleIndex {
  type T = (Int, Float)
  val position = tuple._1
  val value = tuple._2
  val correction = 0
}

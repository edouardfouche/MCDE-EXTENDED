package io.github.edouardfouche.index.tuple

case class CorrectedRankTupleIndex(tuple: (Int, Float, Double)) extends TupleIndex {
  type T = (Int, Float, Double)

  val position = tuple._1
  val rank = tuple._2
  val correction = tuple._3
}

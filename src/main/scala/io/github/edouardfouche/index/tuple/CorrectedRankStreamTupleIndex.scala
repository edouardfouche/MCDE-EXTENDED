package io.github.edouardfouche.index.tuple

case class CorrectedRankStreamTupleIndex(tuple: (Int, Double, Float, Double)) extends TupleIndex {
  type T = (Int, Double, Float, Double)

  val position = tuple._1
  val value = tuple._2
  val adjustedrank = tuple._3
  val correction = tuple._4

  override def toString = s"($position,$value,$adjustedrank,$correction)"
}

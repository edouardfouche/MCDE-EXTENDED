package io.github.edouardfouche.index.tuple

case class RankStreamTupleIndex(tuple: (Int, Double)) extends TupleIndex {
  type T = (Int, Double)
  val position = tuple._1
  val value = tuple._2
  val correction = 0
  //val value = 0

  override def toString = s"($position,$value,X)"
}

package io.github.edouardfouche.index.deprecated

import io.github.edouardfouche.index.deprecated.tuple.TupleIndex

case class AdjustedRankTupleIndex(tuple: (Int, Float)) extends TupleIndex {
  type T = (Int, Float)
  val position: Int = tuple._1
  val value: Float = tuple._2
  val adjustedrank = 0
  val correction = 0

  override def toString = s"($position,$value,X)"
}

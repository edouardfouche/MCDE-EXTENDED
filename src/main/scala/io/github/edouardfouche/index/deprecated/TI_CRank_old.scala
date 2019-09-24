package io.github.edouardfouche.index.deprecated

import io.github.edouardfouche.index.tuple.TupleIndex

case class TI_CRank_old(tuple: (Int, Float, Double)) extends TupleIndex {
  type T = (Int, Float, Double)

  val position: Int = tuple._1
  val value: Float = tuple._2
  val adjustedrank = 0
  val correction: Double = tuple._3

  override def toString = s"($position,$value,$correction)"
}

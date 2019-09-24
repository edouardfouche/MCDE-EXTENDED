package io.github.edouardfouche.index.deprecated

import io.github.edouardfouche.index.tuple.TupleIndex

case class CountTupleIndex_old(tuple: (Int, Array[Int])) extends TupleIndex {
  type T = (Int, Array[Int]) // For each category, we get a list of indexes
  val position: Int = tuple._1
  val value = 0 // tuple._2
  val adjustedrank = 0
  val correction = 0

  override def toString = s"($position,$value,X)"
}

package io.github.edouardfouche.index.deprecated

import io.github.edouardfouche.index.deprecated.tuple.TupleIndex

case class TI_Rank_old(tuple: Int) extends TupleIndex {
  type T = Int
  val position: Int = tuple
  val value = 0
  val adjustedrank = 0
  val correction = 0
  //val value = 0

  override def toString = s"($position,X,X)"
}

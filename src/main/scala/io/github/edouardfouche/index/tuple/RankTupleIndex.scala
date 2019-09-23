package io.github.edouardfouche.index.tuple

case class RankTupleIndex(tuple: Int) extends TupleIndex {
  type T = Int
  val position = tuple
  val value = 0
  val correction = 0
  //val value = 0

  override def toString = s"($position,X,X)"
}

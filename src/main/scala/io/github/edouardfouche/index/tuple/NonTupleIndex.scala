package io.github.edouardfouche.index.tuple

case class NonTupleIndex(tuple: Double) extends TupleIndex {
  type T = Double
  val position = 0
  val value = tuple
  val rank = tuple.toFloat
  val correction = tuple
}

package io.github.edouardfouche.index.tuple

case class NonTupleIndex(tuple: Double) extends TupleIndex {
  type T = AnyVal
  val position = 0
  val value = tuple
  val correction = 0.0
}

package io.github.edouardfouche.index.tuple

trait TupleIndex {
  type T
  val tuple: T

  val position: Int
  val rank: Float
  val correction: Double

  //val value: Double
}
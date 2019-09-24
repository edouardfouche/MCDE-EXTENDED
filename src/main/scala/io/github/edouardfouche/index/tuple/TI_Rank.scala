package io.github.edouardfouche.index.tuple

import scala.language.implicitConversions

case class TI_Rank(tuple: (Int, Double)) extends TupleIndex {
  type T = (Int, Double)
  val position: Int = tuple._1
  val value: Double = tuple._2
  //val adjustedrank = 0
  //val correction = 0

  override def toString = s"($position,$value,X)"

  def toTuple: (Int, Double) = (position, value)

  //implicit def TupleIndexToTuple(input : TI_Rank) : input.T = (input.position, input.value)
}

object TI_Rank {
  implicit def TupleIndexToTuple(input: TI_Rank): (Int, Double) =
    (input.position, input.value)
}
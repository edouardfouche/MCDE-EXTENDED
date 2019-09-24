package io.github.edouardfouche.index.tuple

import scala.language.implicitConversions

case class T_Rank(tuple: (Int, Double)) extends TupleIndex {
  type T = (Int, Double)
  val position: Int = tuple._1
  val value: Double = tuple._2
  //val adjustedrank = 0
  //val correction = 0

  override def toString = s"($position,$value,X)"

  def toTuple: (Int, Double) = (position, value)

  //implicit def TupleIndexToTuple(input : T_Rank) : input.T = (input.position, input.value)
}

object T_Rank {
  implicit def TupleIndexToTuple(input: T_Rank): (Int, Double) =
    (input.position, input.value)
}
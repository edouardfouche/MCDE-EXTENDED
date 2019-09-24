package io.github.edouardfouche.index.tuple

import scala.language.implicitConversions

case class T_CRank(tuple: (Int, Double, Float, Double)) extends TupleIndex {
  type T = (Int, Double, Float, Double)

  val position: Int = tuple._1
  val value: Double = tuple._2
  val adjustedrank: Float = tuple._3
  val correction: Double = tuple._4

  override def toString = s"($position,$value,$adjustedrank,$correction)"

  def toTuple: (Int, Double, Float, Double) = (position, value, adjustedrank, correction)

  //implicit def TupleIndexToTuple(input : T_CRank) : (Int, Double, Float, Double) =
  //  (input.position, input.value, input.adjustedrank, input.correction)
}

object T_CRank {
  implicit def TupleIndexToTuple(input: T_CRank): (Int, Double, Float, Double) =
    (input.position, input.value, input.adjustedrank, input.correction)
}
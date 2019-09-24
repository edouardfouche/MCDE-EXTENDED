package io.github.edouardfouche.index.tuple

import scala.language.implicitConversions

case class T_Dummy(tuple: Double) extends TupleIndex {
  type T = Double
  //val position = 0
  val value: Double = tuple
  //val adjustedrank = 0
  //val correction = 0.0

  override def toString = s"($value)"

  def toTuple: Double = value

  //implicit def TupleIndexToTuple(input : T_Dummy) : input.T = (input.value)
}

object T_Dummy {
  implicit def TupleIndexToTuple(input: T_Dummy): Double =
    input.value
}
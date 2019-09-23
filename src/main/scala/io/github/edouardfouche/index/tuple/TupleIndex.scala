package io.github.edouardfouche.index.tuple
import scala.language.implicitConversions

trait TupleIndex {
  type T
  val tuple: T

  val position: Int
  val value: Any
  val correction: Double

  //val value: Double

  def toString: String

  def toTuple: (Int, Any, Double) = (position,value,correction)
}

object TupleIndex {
  implicit def TupleIndexToTuple(input : TupleIndex) : (Int, Any, Double) = (input.position, input.value, input.correction)
}
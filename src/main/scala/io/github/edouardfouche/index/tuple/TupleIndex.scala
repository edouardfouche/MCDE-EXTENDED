package io.github.edouardfouche.index.tuple
import scala.language.implicitConversions

trait TupleIndex {
  type T
  val tuple: T

  // position: Int
  //val value: Double
  //val adjustedrank: Float
  //val correction: Double

  //val value: Double

  def toString: String

  //def toTuple: (Int, Double, Float, Double) = (position,value,adjustedrank,correction)
  //implicit def TupleIndexToTuple(input : _ <: TupleIndex) : input.T
}

//object TupleIndex {
//  implicit def TupleIndexToTuple(input : TupleIndex) : (Int, Double, Float, Double) = (input.position, input.value, input.adjustedrank, input.correction)
//}
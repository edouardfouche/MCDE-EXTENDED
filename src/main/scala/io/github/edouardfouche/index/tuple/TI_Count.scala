package io.github.edouardfouche.index.tuple
import scala.language.implicitConversions

case class TI_Count(tuple: scala.collection.mutable.Map[Double, (Array[Int], Int)]) extends TupleIndex {
  type T = scala.collection.mutable.Map[Double, (Array[Int], Int)] // For each category, we get a list of indexes
  val map: scala.collection.mutable.Map[Double, (Array[Int], Int)] = tuple
  //val position = 0
  //val value =
  //val adjustedrank = 0
  //val correction = 0

  //def toMap : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map

  //def apply(i: Int) = map(i)
  //def getOrElse(i: Int, k: Int) = map.getOrElse(i,k)

  //implicit def TupleIndexToMap(input : TI_Count) : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map

  override def toString = s"Map(${map.map{case (x,y) => s"$x -> ([${y._1.mkString(",")}], ${y._2})"}})"

  //implicit def TupleIndexToTuple(input : TI_CRank) : (Int, Double, Float, Double) =
  //  (input.position, input.value, input.adjustedrank, correction)
}

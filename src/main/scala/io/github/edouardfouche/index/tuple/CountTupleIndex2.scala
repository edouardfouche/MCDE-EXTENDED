package io.github.edouardfouche.index.tuple
import scala.language.implicitConversions

case class CountTupleIndex2(tuple: scala.collection.mutable.Map[Double, (Array[Int], Int)]) extends TupleIndex {
  type T = scala.collection.mutable.Map[Double, (Array[Int], Int)] // For each category, we get a list of indexes
  val map = tuple
  val position = 0
  val value = 0
  val correction = 0

  def toMap : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map
  implicit def TupleIndexToMap(input : CountTupleIndex2) : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map

  override def toString = s"Map(${map.map{case (x,y) => s"$x -> ([${y._1.mkString(",")}], ${y._2})"}})"
}

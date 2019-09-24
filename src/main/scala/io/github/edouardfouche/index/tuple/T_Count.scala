package io.github.edouardfouche.index.tuple

import scala.language.implicitConversions

case class T_Count(tuple: (Array[Int], Int)) extends TupleIndex {
  type T = (Array[Int], Int) // For each category, we get a list of indexes
  //val map: scala.collection.mutable.Map[Double, (Array[Int], Int)] = tuple
  //val position = 0
  //val value =
  //val adjustedrank = 0
  //val correction = 0

  //def toMap : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map

  //def apply(i: Int) = map(i)
  //def getOrElse(i: Int, k: Int) = map.getOrElse(i,k)

  //implicit def TupleIndexToMap(input : T_Count) : scala.collection.mutable.Map[Double, (Array[Int], Int)] = map

  override def toString = s"[${tuple._1.mkString(",")}], ${tuple._2})"

  implicit def TupleIndexToTuple(input: T_Count): (Array[Int], Int) =
    (input.tuple._1, input.tuple._2)
}

object T_Count {
  implicit def TupleIndexToTuple(input: T_Count): (Array[Int], Int) =
    (input.tuple._1, input.tuple._2)
}
package io.github.edouardfouche.index.deprecated.tuple

import scala.language.implicitConversions

case class T_Count(tuple: (scala.collection.immutable.Queue[Int], Int)) extends TupleIndex {
  type T = (scala.collection.immutable.Queue[Int], Int) // For each category, we get a list of indexes
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

  //def decrementQueue(offset: Int): Unit = {
  //  tuple._1 = tuple._1.map(x => x - offset)
  //}

  //implicit def TupleIndexToTuple(input: T_Count): (scala.collection.immutable.Queue[Int], Int) =
  //  (input.tuple._1, input.tuple._2)
}

object T_Count {
  implicit def TupleIndexToTuple(input: T_Count): (scala.collection.immutable.Queue[Int], Int) =
    (input.tuple._1, input.tuple._2)
}
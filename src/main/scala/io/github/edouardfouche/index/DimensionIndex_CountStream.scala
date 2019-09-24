/*
 * Copyright (C) 2018 Edouard Fouché
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package io.github.edouardfouche.index

import io.github.edouardfouche.index.tuple.{CountTupleIndex, CountTupleIndex2, RankStreamTupleIndex}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  * @param values
  */
class DimensionIndex_CountStream(val values: Array[Double]) extends DimensionIndex {
  type T = CountTupleIndex2

  //val group: Map[Double, Array[(Double,Int)]]  = values.zipWithIndex.groupBy(_._1)
  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double]()
  var offset = 0

  // that is the two important things
  //val indexes: collection.mutable.Map[Double, Array[Int]] =
  //  collection.mutable.Map[Double, Array[Int]]() ++= values.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.map(_._2))})
  //val counts: collection.mutable.Map[Double, Int] =
  //  collection.mutable.Map[Double, Int]() ++=indexes.mapValues(_.length)

  //var categories: ArrayBuffer[Double] = ArrayBuffer[Double](counts.keys.toArray: _*) // don't need to sort

  //val int_to_value: Map[Int, Double] = categories.zipWithIndex.toMap.map({case (x,y) => (y,x)})
  //def categorie_to_position: Map[Double, Int] = categories.zipWithIndex.toMap

  var dindex: Array[T] = createDimensionIndex(values)

  def refresh: Unit = {
    if(offset > 0) {
      dindex(0).map.keys.foreach(x => dindex(0).map(x) = (dindex(0).map(x)._1.map(y => y-offset), dindex(0).map(x)._2))
    }
    offset = 0
  }

  def insert(newpoint: Double): Unit = {

    val todelete = queue.dequeue()
    // handle insertion
    if(dindex(0).map.getOrElse(newpoint, -1) != -1) { // in that case we already have an entry for this category
      dindex(0).map(newpoint) = (dindex(0).map(newpoint)._1 :+ values.length+offset, dindex(0).map(newpoint)._2 + 1)
    } else {
      // Handle the case were this is a new category
      dindex(0).map(newpoint) = (Array(values.length+offset), 1)
    }
    // handle deletion
    if(dindex(0).map(todelete)._2 > 1) { // In that case we don't need to remove the entry
      val position = dindex(0).map(todelete)._1.zipWithIndex.min._2
      dindex(0).map(todelete) = (dindex(0).map(todelete)._1.take(position) ++ dindex(0).map(todelete)._1.drop(position+1),
        dindex(0).map(todelete)._2 -1)
    } else {
      dindex(0).map.remove(todelete)
      // For those guys, we need to find where it is.
      //val postodelete = categories.zipWithIndex.filter(_._1 == todelete)(0)._2
      //categories = categories.take(postodelete) ++ categories.drop(postodelete+1)
      //dindex = dindex.take(postodelete) ++ dindex.drop(postodelete+1)
    }
    offset += 1
    queue += newpoint
  }
  def insertreplace(newdata: Array[Double]): Unit = {}

  def createDimensionIndex(input: Array[Double]): Array[T] = {
    // somewhat inefficient to do that twice, fix that
    //val group: Map[U, Vector[(U,Int)]]  = values.zipWithIndex.groupBy(_._1)
    //val counts: Map[U, Int] = group.map({case (x,y) => (x,y.length)})
    //val categories: Vector[U] = values.distinct.sorted

    //val int_to_value: Map[Int, U] = categories.zipWithIndex.toMap.map({case (x,y) => (y,x)}) // map  each category to location in index

    //val indexes: Map[Double, Array[Int]] = group.map({case (x,y) => (x,y.map(_._2).toArray)})
    //categories.indices.toArray.map(x =>
    //  CountTupleIndex(x, indexes(x))
    //)

    //categories.zipWithIndex.map(x => CountTupleIndex(x._2, indexes(x._1))).toArray
    input.foreach(x => queue += x)
    Array(CountTupleIndex2(
      collection.mutable.Map(values.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,(y.map(_._2),y.length))}).toSeq: _*)
    ))
  }

  def selectCategories(sliceSize: Int): Array[Double] = {
    val shuffledCategories: List[Double] = scala.util.Random.shuffle(dindex(0).toMap.keys.toList)//.take(sliceSize)

    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[Double], selectedCategories: List[Double]): List[Double] = {
      // make sure there is a least one category that is not selected{
      if(categories.length == 1) selectedCategories
      if(current < sliceSize) {
        if(selectedCategories.isEmpty // make sure we select at least one category
          || (math.abs(current + dindex(0).toMap(categories.head)._2 - sliceSize) < math.abs(current - sliceSize)))  // make sure we get the closest match to the desired sliceSize
        {
          cumulative(current + dindex(0).toMap(categories.head)._2, categories.tail, selectedCategories :+ categories.head)
        } else selectedCategories
      } else selectedCategories
    }
    cumulative(0, shuffledCategories, List[Double]()).toArray
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(false)
    //TODO: Problem: The categories might as well not be uniform. So deciding the number of categories is misleading
    //TODO: Check that it works now.
    /*
    val shuffledCategories: List[Int] = scala.util.Random.shuffle(categories.indices.toList)//.take(sliceSize)
    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[Int], selectedCategories: List[Int]): List[Int] = {
      if(current < sliceSize && categories.nonEmpty) {
        cumulative(current + counts(int_to_value(categories.head)), categories.tail, selectedCategories :+ categories.head)
      } else selectedCategories
    }
    val selectedCategories = cumulative(0, shuffledCategories, List[Int]())
    */
    val selectedCategories: Array[Double] = selectCategories(sliceSize)
    val selectedIndexes: Array[Int] = selectedCategories.flatMap(x => dindex(0).toMap(x)._1)
    //val nonselectedIndexes: Set[Int] = values.indices.toSet -- selectedIndexes
    selectedIndexes.foreach(x => logicalArray(x) = true)
    logicalArray
  }

  /*
  /**
    * Return the count index structure as required for the chi-squared test in MCDE framework
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of 2-D Tuple, where the first element is the original index, the second is the count of its elements.
    */
  def csCount(input: Vector[U]): Array[CountTupleIndex] = {
    //val order = scala.util.Random.shuffle(input.indices.toList)
    val counts = input.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.length)})
    val categories = counts.keys
    val categories_order = scala.util.Random.shuffle(categories).zipWithIndex.toMap

    input.zipWithIndex.sortBy(x => categories_order(x._1)).map(x => CountTupleIndex(x._2, x._1.toString)).toArray
  }
  */

}

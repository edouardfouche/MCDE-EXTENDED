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

import io.github.edouardfouche.index.tuple.{CountTupleIndex, TupleIndex}

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  * @param values
  */
class DimensionIndex_Count(val values: Array[Double]) extends DimensionIndex {
  type T = CountTupleIndex

  //val group: Map[Double, Array[(Double,Int)]]  = values.zipWithIndex.groupBy(_._1)

  // that is the two important things
  val indexes: Map[Double, Array[Int]] = values.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.map(_._2))})
  val counts: Map[Double, Int] = indexes.mapValues(_.length)

  val categories: Array[Double] = counts.keys.toArray.sorted

  //val int_to_value: Map[Int, Double] = categories.zipWithIndex.toMap.map({case (x,y) => (y,x)})
  val categorie_to_position: Map[Double, Int] = categories.zipWithIndex.toMap
  val position_to_categorie: Map[Int, Double] = categorie_to_position.map({case (x,y) => (y,x)})

  var dindex: Array[T] = createDimensionIndex(values)

  // TODO
  def insert(newdata: Array[Double]): Unit = {}
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
    categories.map(x => CountTupleIndex(categorie_to_position(x), indexes(x)))
  }

  def selectCategories(sliceSize: Int): Array[Double] = {
    val shuffledCategories: List[Double] = scala.util.Random.shuffle(categories.toList)//.take(sliceSize)

    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[Double], selectedCategories: List[Double]): List[Double] = {
      if(current < sliceSize && categories.nonEmpty) {
        cumulative(current + counts(categories.head), categories.tail, selectedCategories :+ categories.head)
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
    val selectedIndexes: Array[Int] = selectedCategories.flatMap(x => dindex(categorie_to_position(x)).value)
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

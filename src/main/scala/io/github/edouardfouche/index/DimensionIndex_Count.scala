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
class DimensionIndex_Count[U](val values: Vector[U])(implicit ord: U => Ordered[U]) extends DimensionIndex[U] {
  type T = CountTupleIndex

  val group: Map[U, Vector[(U,Int)]]  = values.zipWithIndex.groupBy(_._1)

  val counts: Map[U, Int] = group.map({case (x,y) => (x,y.length)})
  val categories: Vector[U] = values.distinct.sorted

  val int_to_value: Map[Int, U] = categories.zipWithIndex.toMap.map({case (x,y) => (y,x)}) // map  each category to location in index

  def createDimensionIndex(input: Vector[U]): Array[T] = {
    val indexes: Map[U, Array[Int]] = group.map({case (x,y) => (x,y.map(_._2).toArray)})
    categories.indices.toArray.map(x => CountTupleIndex(x, indexes(int_to_value(x))))
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val selectedCategories: List[Int] = scala.util.Random.shuffle(categories.indices.toList).take(sliceSize)
    for {x <- selectedCategories} {
      val selectedIndexes: Array[Int] = dindex(x).value
      for {y <- selectedIndexes} {
        logicalArray(y) = false
      }
    }
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

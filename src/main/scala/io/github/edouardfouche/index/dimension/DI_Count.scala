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
package io.github.edouardfouche.index.dimension

import io.github.edouardfouche.index.tuple.TI_Count

/**
  * A very simple index structure will only the ranks and values(convenient for HiCS for example)
  *
  * @param values An array of values corresponding to the values in a column
  */
class DI_Count(val values: Array[Double]) extends DimensionIndex {
  type T = TI_Count

  var dindex: Array[T] = createDimensionIndex(values)

  //override def apply(i: Int): (Array[Int], Int) = dindex(0).map(i)

  def createDimensionIndex(input: Array[Double]): Array[T] = {
    Array(TI_Count(
      collection.mutable.Map(values.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,(y.map(_._2),y.length))}).toSeq: _*)
    ))
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(false)
    //TODO: Problem: The categories might as well not be uniform. So deciding the number of categories is misleading
    //TODO: Check that it works now.
    val selectedCategories: Array[Double] = selectCategories(sliceSize)
    val selectedIndexes: Array[Int] = selectedCategories.flatMap(x => dindex(0).map(x)._1)
    selectedIndexes.foreach(x => logicalArray(x) = true)
    logicalArray
  }

  def selectCategories(sliceSize: Int): Array[Double] = {
    val shuffledCategories: List[Double] = scala.util.Random.shuffle(dindex(0).map.keys.toList) //.take(sliceSize)

    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[Double], selectedCategories: List[Double]): List[Double] = {
      // make sure there is a least one category that is not selected
      if(categories.length == 1) selectedCategories
      if(current < sliceSize) {
        if(selectedCategories.isEmpty // make sure we select at least one category
          || (math.abs(current + dindex(0).map(categories.head)._2 - sliceSize) < math.abs(current - sliceSize))) // make sure we get the closest match to the desired sliceSize
        {
          cumulative(current + dindex(0).map(categories.head)._2, categories.tail, selectedCategories :+ categories.head)
        } else selectedCategories
      } else selectedCategories
    }
    cumulative(0, shuffledCategories, List[Double]()).toArray
  }
}

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

import scala.collection.mutable

/**
  * A very simple index structure will only the ranks and values(convenient for HiCS for example)
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_Count(val initvalues: Array[Double]) extends DimensionIndex {
  //type T = (scala.collection.immutable.Queue[Int], Int) // First element is a queue of the index of a given value in the map, second element in the size of this queue.
  type T = (Array[Int], Int)
  val id = "Count"
  var currentvalues = initvalues

  //var dindex: Array[T] = createDimensionIndex(values)
  var dindex: mutable.Map[Double, T] = createDimensionIndex(initvalues)

  def apply(i: Double): (Array[Int], Int) = dindex(i)

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    currentvalues = currentvalues.drop(1) ++ Array(newpoint)
    dindex = createDimensionIndex(currentvalues)
  }

  def createDimensionIndex(input: Array[Double]): mutable.Map[Double, T] = {
    //Array(T_Count(
    //  collection.mutable.Map(values.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,(y.map(_._2),y.length))}).toSeq: _*)
    //))
    //collection.mutable.Map(values.zipWithIndex.groupBy(_._1).map({ case (x, y) => (x, T_Count(y.map(_._2), y.length)) }).toSeq: _*)

    val map = mutable.Map[Double, (Array[Int], Int)]()
    for {
      x <- input.indices
    } {
      val current: (Array[Int], Int) = map.getOrElse(input(x),
        (Array[Int](), 0))
      val c: Array[Int] = current._1
      map(input(x)) = (c :+ x, current._2 + 1) // we use a queue so that we can know efficiently which to delete.
    }
    map.map({ case (x, y) => (x, (y._1, y._2)) })
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(false)
    val selectedCategories: Array[Double] = selectSlice(sliceSize)
    val selectedIndexes: Array[Int] = selectedCategories.flatMap(x => dindex(x)._1)

    // Correcting the slice size
    val c_selectedIndexes: Array[Int] = if (selectedIndexes.length > sliceSize) {
      scala.util.Random.shuffle(selectedIndexes.toList).drop(selectedIndexes.length - sliceSize).toArray
    } else if (selectedIndexes.length < sliceSize) {
      val othercategories = dindex.keys.filter(!selectedCategories.contains(_))
      val otherindexes = othercategories.flatMap(x => dindex(x)._1)
      selectedIndexes ++ scala.util.Random.shuffle(otherindexes.toList).take(sliceSize - selectedIndexes.length)
    } else selectedIndexes

    c_selectedIndexes.foreach(x => logicalArray(x) = true)
    logicalArray
  }

  def uniformslice(sliceSize: Int): Array[Boolean] = slice(sliceSize)

  /*
  def selectCategories(sliceSize: Int): Array[Double] = {
    val shuffledCategories: List[Double] = scala.util.Random.shuffle(dindex.keys.toList) //.take(sliceSize)

    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[Double], selectedCategories: List[Double]): List[Double] = {
      // make sure there is a least one category that is not selected
      if(categories.length == 1) selectedCategories
      if(current < sliceSize) {
        if(selectedCategories.isEmpty // make sure we select at least one category
          || (math.abs(current + dindex(categories.head)._2 - sliceSize) < math.abs(current - sliceSize))) // make sure we get the closest match to the desired sliceSize
        {
          cumulative(current + dindex(categories.head)._2, categories.tail, selectedCategories :+ categories.head)
        } else selectedCategories
      } else selectedCategories
    }
    cumulative(0, shuffledCategories, List[Double]()).toArray
  }
  */


  def selectSlice(sliceSize: Int): Array[Double] = {
    val ratio = sliceSize.toDouble / initvalues.length.toDouble
    val categories = dindex.keys
    val toselect: Int = math.floor(categories.size * ratio).toInt.max(1).min(categories.size - 1) // Make sure at least 1, a most ncategories - 1
    scala.util.Random.shuffle(categories.toList).take(toselect).toArray
  }

  def selectRestriction(sliceSize: Int): Array[Double] = {
    val ratio = sliceSize.toDouble / initvalues.length.toDouble
    val categories = dindex.keys
    val toselect: Int = math.floor(categories.size * ratio).toInt.max(2).min(categories.size) // Make sure at least 2, a most ncategories
    scala.util.Random.shuffle(dindex.keys.toList).take(toselect).toArray
  }
}

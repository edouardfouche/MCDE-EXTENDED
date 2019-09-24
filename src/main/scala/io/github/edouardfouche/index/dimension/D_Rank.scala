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

import io.github.edouardfouche.index.tuple.T_Rank

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  *
  * @param values An array of values corresponding to the values in a column
  */
class D_Rank(val values: Array[Double]) extends DimensionIndex {
  type T = T_Rank

  var dindex: Array[T] = createDimensionIndex(values)

  def apply(n: Int): T = dindex(n) // access in the index

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    dindex = createDimensionIndex(values.drop(1) ++ Array(newpoint))
  }

  def createDimensionIndex(input: Array[Double]): Array[T] = {
    input.zipWithIndex.sortBy(_._1).map(x => T_Rank(x._2, x._1))
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val sliceStart = scala.util.Random.nextInt((length - sliceSize).max(1))
    for {x <- 0 until sliceStart} {
      logicalArray(dindex(x).position) = false
    }
    for {x <- sliceStart + sliceSize until dindex.length} {
      logicalArray(dindex(x).position) = false
    }
    logicalArray
  }
}
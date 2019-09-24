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

import io.github.edouardfouche.index.tuple.TupleIndex

abstract class DimensionIndex {
  val values: Array[Double] // An array of values corresponding to the values in a column
  type T <: TupleIndex
  var dindex: Array[T]

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    dindex = createDimensionIndex(values.drop(1) ++ Array(newpoint))
  }

  def refresh(): Unit = {} // Do nothing, DimensionIndexStream must override

  def indices: Range = dindex.indices // this is supposed to give the indices of the columns

  def apply(n: Int): T = dindex(n) // access in the index

  def length: Int = values.length

  def slice(sliceSize: Int): Array[Boolean]

  def isEmpty: Boolean = values.length == 0

  override def toString: String = dindex mkString ";"

  /**
    * @param data An array of values corresponding to the values in a column
    * @return An index, which is also column-oriented
    */
  protected def createDimensionIndex(data: Array[Double]): Array[T]
}

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

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  *
  * @param values An array of values corresponding to the values in a column
  */
class D_Rank(val values: Array[Double]) extends DimensionIndex {
  val id = "Rank"
  type T = (Int, Double) // T_Rank
  // first element (Int) -> position
  // second elements (Double) -> value

  var dindex: Array[T] = createDimensionIndex(values)

  def apply(n: Int): T = dindex(n) // access in the index

  override def toString: String = dindex mkString ";"

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    dindex = createDimensionIndex(values.drop(1) ++ Array(newpoint))
  }

  def createDimensionIndex(input: Array[Double]): Array[T] = {
    //input.zipWithIndex.sortBy(_._1).map(x => (x._2, x._1))
    // tie breaking random list
    // I had the idea for this trick from here: https://stackoverflow.com/questions/44440018/handling-scala-array-group-with-ties
    val rd = scala.util.Random.shuffle(input.indices.toList) // tie breaking random list
    input.zipWithIndex.zip(rd).map(x => (x._1._1, x._1._2, x._2)).
      sortWith((x, y) => (x._1 < y._1) || ((x._1 == y._1) && x._3 < y._3)).map(x => (x._2, x._1))
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val sliceStart = scala.util.Random.nextInt((length - sliceSize).max(1))
    for {x <- 0 until sliceStart} {
      logicalArray(dindex(x)._1) = false
    }
    for {x <- sliceStart + sliceSize until dindex.length} {
      logicalArray(dindex(x)._1) = false
    }

    //println(s"sliceSize= $sliceSize")
    logicalArray
  }
}

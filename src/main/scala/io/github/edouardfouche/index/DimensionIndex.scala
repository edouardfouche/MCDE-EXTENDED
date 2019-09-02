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

import io.github.edouardfouche.index.tuple.TupleIndex

import scala.annotation.tailrec

trait DimensionIndex[U] {
  val values: Array[U]

  val dindex: Array[_ <: TupleIndex] = createIndex(values)

  /**
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createIndex(data: Array[U]): Array[_ <: TupleIndex]

  def apply(n: Int) = dindex(n) // access in the index

  def indices = dindex.indices // this is supposed to give the indices of the columns

  def length = dindex.length

  def isEmpty: Boolean = dindex.length == 0

  def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val sliceStart = scala.util.Random.nextInt((length - sliceSize).max(1))
    for {x <- 0 until sliceStart} {logicalArray(dindex(x).position) = false}
    for {x <- sliceStart + sliceSize until dindex.length} {logicalArray(dindex(x).position) = false}
    logicalArray
  }

  /**
  //TODO: Question : Is it problematic to slice on ties? Its seems not.
  def randomSlice(dimensions: Set[Int], referenceDim: Int, sliceSize: Int): Array[Boolean]

  def allSlice(dimensions: Set[Int], sliceSize: Int): Array[Boolean]

  def safeSlice(dimensions: Set[Int], referenceDim: Int, sliceSize: Int): Array[Boolean]

  // the slicing scheme used for conditional independence
  def simpleSlice(dimension: Int, sliceSize: Int): this.type

  def getSafeCut(cut: Int, reference: Int): Int

  def mean(xs: Array[Int]): Float = xs.sum / xs.length.toFloat

  def restrictedSafeRandomSlice(dimensions: Set[Int], referenceDim: Int, alpha: Double): Array[Boolean]

  def restrictedRandomSlice(dimensions: Set[Int], referenceDim: Int, alpha: Double): Array[Boolean]

  @tailrec
  final def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def areCoPrimes(a: Int, b: Int): Boolean = gcd(a, b) == 1

  def closeSearch(a: Int, inc: Int = 1): Stream[Int] = {
    //(a+1) #:: closeSearch(a+1)
    if (a - inc > 1) (a + inc) #:: (a - inc) #:: closeSearch(a, inc + 1)
    else (a + inc) #:: closeSearch(a, inc + 1)
  }

  def findClosestCoPrime(a: Int, ref: Int): Int = {
    if (areCoPrimes(a, ref)) a
    else closeSearch(a).filter(areCoPrimes(_, ref)).head
  }
    */
}

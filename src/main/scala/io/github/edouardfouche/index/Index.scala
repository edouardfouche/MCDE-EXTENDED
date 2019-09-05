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

import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec

abstract class Index {
  //val values: DataSet
  val data: DataSet
  val parallelize:Int

  type T <: DimensionIndex[_]
  val index: Array[T] = createIndex(data)


  /**
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createIndex(data: DataSet): Array[T]


  def apply(n: Int): T = index(n) // access the columns of the index

  def indices = (0 until data.ncols) // this is supposed to give the indices of the columns

  def ncols = data.ncols
  def nrows = data.nrows

  def isEmpty: Boolean = data.ncols == 0



  /**
    * Produce a subspace slice by conditioning on all dimensions, except a reference dimension
    * @param dimensions The set of dimensions of the subspaces
    * @param referenceDim The dimension that is considered as reference
    * @param sliceSize The size of the slice for each dimensions, determined by alpha
    * @return Returns an array of booleans. True corresponds to indexes included in the slice.
    */
  //TODO: Question : Is it problematic to slice on ties? Its seems not.
  def randomSlice(dimensions: Set[Int], referenceDim: Int, sliceSize: Int): Array[Boolean] = {
    dimensions.filter(_ != referenceDim).map(x => index(x).slice(sliceSize)).toArray.transpose.map(x => !x.contains(false))
  }

  def getSafeCut(cut: Int, reference: Int): Int = {
    //require(cut >= 0 & cut <= reference.length)
    val ref = index(reference)
    //println(s"ref.length: ${ref.length}: ref($cut): ${ref(cut)} : ref(${cut+1}): ${ref(cut+1)}")
    @tailrec def cutSearch(a: Int, inc: Int = 0, ref: DimensionIndex[_]): Int = {
      // "It's easier to ask forgiveness than it is to get permission"
      try if(ref(a+inc).value != ref(a+inc-1).value) return a+inc
      else {
        try if (ref(a - inc).value != ref(a - inc - 1).value) return a - inc
        catch{case _: Throwable => return a-inc}
      }
      catch {case _: Throwable => return a+inc}
      cutSearch(a, inc+1, ref)
    }
    cutSearch(cut, 0, ref)
  }

  /**
    * Find the greatest common divisor of a and b
    * @param a integer
    * @param b integer
    * @return An integer, the greatest common divisor of a and b
    */
  @tailrec
  final def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  /**
    * Coprimality test between two integers a and b
    * @param a integer
    * @param b integer
    * @return boolean, true if a and b are coprimes
    */
  def areCoPrimes(a: Int, b: Int): Boolean = gcd(a, b) == 1

  /**
    * Helper function that generates a stream of integers that are close to a, shall not be smaller than 1
    * @param a starting integer
    * @param inc increment (internal parameter with default value 1)
    * @return A stream of integer close to a and greater or equal to 1
    */
  def closeSearch(a: Int, inc: Int = 1): Stream[Int] = {
    //(a+1) #:: closeSearch(a+1)
    if (a - inc > 1) (a + inc) #:: (a - inc) #:: closeSearch(a, inc + 1)
    else (a + inc) #:: closeSearch(a, inc + 1)
  }

  /**
    * Find the closest integer from a, which is coprime with ref
    * @param a An integer. We want to find the closest coprime from this position
    * @param ref A reference integer.
    * @return The cloest integer from a which is coprime with ref
    */
  def findClosestCoPrime(a: Int, ref: Int): Int = {
    if (areCoPrimes(a, ref)) a
    else closeSearch(a).filter(areCoPrimes(_, ref)).head
  }
}

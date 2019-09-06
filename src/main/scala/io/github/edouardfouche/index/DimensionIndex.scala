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

import io.github.edouardfouche.index.tuple.{AdjustedRankTupleIndex, CorrectedRankTupleIndex, RankTupleIndex, TupleIndex, CountTupleIndex}

import scala.annotation.tailrec

// [U](implicit ord: U => Ordered[U])
abstract class DimensionIndex {
  //implicit protected val cmp: Ordering[_ >: U];
  type U = String
  val values: Vector[U]
  type T <: TupleIndex

  var dindex: Array[T]

  /**
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createDimensionIndex(data: Vector[U]): Array[T]

  def insert(newdata: Vector[U]): Unit

  def insertreplace(newdata: Vector[U]): Unit

  def apply(n: Int): T = dindex(n) // access in the index

  def indices = dindex.indices // this is supposed to give the indices of the columns

  def length = values.length

  def isEmpty: Boolean = values.length == 0

  def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val sliceStart = scala.util.Random.nextInt((length - sliceSize).max(1))
    for {x <- 0 until sliceStart} {logicalArray(dindex(x).position) = false}
    for {x <- sliceStart + sliceSize until dindex.length} {logicalArray(dindex(x).position) = false}
    logicalArray
  }

  def getSafeCut(cut: Int): Int = {
    //require(cut >= 0 & cut <= reference.length)
    //val ref = index(reference)
    //println(s"ref.length: ${ref.length}: ref($cut): ${ref(cut)} : ref(${cut+1}): ${ref(cut+1)}")
    @tailrec def cutSearch(a: Int, inc: Int = 0, ref: DimensionIndex): Int = {
      // "It's easier to ask forgiveness than it is to get permission"
      try if(ref(a+inc).value != ref(a+inc-1).value) return a+inc
      else {
        try if (ref(a - inc).value != ref(a - inc - 1).value) return a - inc
        catch{case _: Throwable => return a-inc}
      }
      catch {case _: Throwable => return a+inc}
      cutSearch(a, inc+1, ref)
    }
    cutSearch(cut, 0, this)
  }

  /*
  /**
    * Return the rank index structure for MWP, with adjusted ranks but no correction for ties.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of 2-D Tuple, where the first element is the original index, the second is its rank.
    */
  def mwRank(input: Vector[U]): Array[AdjustedRankTupleIndex] = {
    // Create an index for a column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
    val adjusted = nonadjusted.map(y => AdjustedRankTupleIndex(y._1,y._2)).toArray

    val m = nonadjusted.length - 1
    var j = 0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._3 == nonadjusted(k + 1)._3)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._2
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._2) / (k - j + 1.0)).toFloat
        (j to k).foreach(y => adjusted(y) = AdjustedRankTupleIndex(nonadjusted(y)._1, newval))
        j += k - j + 1 // jump to after the replacement
      } else j += 1
    }
    adjusted
  }
  */


  /*
  /**
    * Return the rank index structure for MWP, with adjusted ranks AND correction for ties.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of 3-D Tuple, where the first element is the original index, the second is its rank and the
    *         the last one a cumulative correction for ties.
    */
  def mwRankCorrectionCumulative(input: Vector[U]): Array[CorrectedRankTupleIndex] = {
    // Create an index for each column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
    val adjusted = new Array[CorrectedRankTupleIndex](input.length)

    val m = nonadjusted.length - 1
    var j = 0
    var acc_corr = 0.0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._3 == nonadjusted(k + 1)._3)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._2
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._2) / (k - j + 1.0)).toFloat
        val t = k - j + 1.0
        acc_corr = acc_corr + math.pow(t , 3) - t
        (j to k).foreach(y => adjusted(y) = CorrectedRankTupleIndex(nonadjusted(y)._1, newval, acc_corr))
        j += k - j + 1 // jump to after the replacement
      } else {
        adjusted(j) = CorrectedRankTupleIndex(nonadjusted(j)._1, nonadjusted(j)._2, acc_corr)
        j += 1
      }
    }

    adjusted
  }
  */


  /*
  /**
    * Return the rank index structure (as in HiCS).
    *
    * Note that the numbers might be different in the case of ties, in comparison with other implementations.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of Int, where the element is the original index in the unsorted data set
    */
  def ksRankSimple(input: Vector[U]): Array[RankTupleIndex] = {
    input.zipWithIndex.sortBy(_._1).map(x => RankTupleIndex(x._2)).toArray
  }
  */




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

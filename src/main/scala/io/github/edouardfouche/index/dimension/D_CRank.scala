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

import io.github.edouardfouche.index.tuple.T_CRank

import scala.annotation.tailrec

/**
  * Compute an adjusted, corrected rank index from a given data set
  * The rank are adjusted, which means that in the case of ties, the rank is defined as the average rank of the tying values
  * Also, a "correction for ties" is computed, as required to compute a Mann-Whitney U test
  * The correction is computed as a cumulative value.
  *
  * @param values An array of values corresponding to the values in a column
  */
class D_CRank(val values: Array[Double]) extends DimensionIndex {
  type T = T_CRank

  var dindex: Array[T] = createDimensionIndex(values)

  def apply(n: Int): T = dindex(n) // access in the index

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    dindex = createDimensionIndex(values.drop(1) ++ Array(newpoint))
  }

  def createDimensionIndex(input: Array[Double]): Array[T]= {
    // Create an index for each column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = {
      input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._1._1, y._2.toFloat, y._1._1))
    }
    val adjusted = new Array[T_CRank](input.length)

    val m = nonadjusted.length - 1
    var j = 0
    var acc_corr = 0.0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._2 == nonadjusted(k + 1)._2)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._3
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._3) / (k - j + 1.0)).toFloat
        val t = k - j + 1.0
        acc_corr = acc_corr + math.pow(t , 3) - t
        (j to k).foreach(y => adjusted(y) = T_CRank(nonadjusted(y)._1, nonadjusted(y)._2, newval, acc_corr))
        j += k - j + 1 // jump to after the replacement
      } else {
        adjusted(j) = T_CRank(nonadjusted(j)._1, nonadjusted(j)._2, nonadjusted(j)._3, acc_corr)
        j += 1
      }
    }

    adjusted
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

  def getSafeCut(cut: Int): Int = {
    //require(cut >= 0 & cut <= reference.length)
    //val ref = index(reference)
    //println(s"ref.length: ${ref.length}: ref($cut): ${ref(cut)} : ref(${cut+1}): ${ref(cut+1)}")
    @tailrec def cutSearch(a: Int, inc: Int = 0, ref: D_CRank): Int = {
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
}
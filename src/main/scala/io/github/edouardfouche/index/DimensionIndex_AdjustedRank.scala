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

import io.github.edouardfouche.index.tuple.{AdjustedRankTupleIndex, TupleIndex}
import io.github.edouardfouche.preprocess.Preprocess

/*
//TODO: Refactor the Slice1, Slice2, Slice3
/**
  * Compute an adjusted rank index from a given data set
  * The "adjusted rank" means that in the case of ties, the rank is defined as the average rank of the tying values
  *
  * @param values A row-oriented data set
  */
class DimensionIndex_AdjustedRank(val values: Vector[String])  extends DimensionIndex  {
  type T = AdjustedRankTupleIndex

  var dindex: Array[T] = createDimensionIndex(values)

  // TODO
  def insert(newdata: Vector[U]): Unit = {}
  def insertreplace(newdata: Vector[U]): Unit = {}

  def createDimensionIndex(input: Vector[U]): Array[T] = {
     val nonadjusted = {
       try{
         input.map(_.toInt).zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
       } catch {
         case _: Throwable  => try{
           input.map(_.toDouble).zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
         } catch {
           case _: Throwable  => input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
         }
       }
     }
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
}
*/


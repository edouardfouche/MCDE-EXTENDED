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

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  *
  * @param values An array of values corresponding to the values in a column
  */
class D_Rank_Stream(values: Array[Double]) extends D_Rank(values) with DimensionIndexStream {
  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double](values: _*)
  var offset: Int = 0

  //override var dindex: Array[T] = createDimensionIndex(values)

  override def refresh: Unit = {
    if(offset > 0) {
      dindex = dindex.map(x => new T_Rank(x.position - offset, x.value))
    }
    offset = 0
  }

  override def insert(newPoint: Double): Unit = {
    val todelete = queue.dequeue
    // the binary search returns the index on the match with oldest position
    // if no match, the index just after
    def binarySearch_insert(start: Int, end: Int, value: Double): Int = {
      @tailrec
      def binarySearch_acc(start: Int, end: Int, value: Double): Int = { // this binary search is good only for finding the point where to insert
        val i = (end+start) / 2
        //println(s"start: $start, end: $end, i:$i")
        (dindex(i).value, dindex(i + 1).value) match {
          case x if (x._1 < value) & (x._2 >= value) =>
            if(x._2 > value) i+1 // in this case, no multiple same value
            else { // in case of match, return position at the oldest index
              var newest = i+1
              var j = newest + 1
              //println(s"some match; current: $newest")
              while((j < dindex.length) && (dindex(j).value == value)) {
                if(dindex(j).position > dindex(newest).position) newest = j
                j+=1
              }
              newest+1
            }
          case x if x._1 >= value => binarySearch_acc(start, i - 1, value)
          case x if x._2 < value => binarySearch_acc(i + 1, end, value)
        }
      }
      if(dindex(0).value > value) 0
      else if(dindex(0).value == value) {
        var newest = 0
        var j = newest + 1
        //println(s"some match; current: $oldest")
        while((j < dindex.length) && (dindex(j).value == value)) {
          if(dindex(j).position > dindex(newest).position) newest = j+1
          j+=1
        }
        newest+1
      }
      else if(dindex(dindex.length-1).value == value) dindex.length-1
      else if(dindex(dindex.length-1).value < value) dindex.length
      else binarySearch_acc( 0, dindex.length - 1, value)
    }

    def binarySearch_delete(start: Int, end: Int, value: Double): Int = {
      @tailrec
      def binarySearch_acc(start: Int, end: Int, value: Double): Int = {
        val i = (end+start) / 2
        //println(s"start: $start, end: $end, i:$i")
        (dindex(i).value, dindex(i + 1).value) match {
          case x if x._1 == value =>
            var oldest = i
            var j = oldest - 1
            while((j >= 0) && (dindex(j).value == value)) {
              if(dindex(j).position < dindex(oldest).position) oldest = j
              j-=1
            }
            oldest
          case x if x._2 == value =>
            var oldest = i+1
            var j = oldest - 1
            while((j >= 0) && (dindex(j).value == value)) {
              if(dindex(j).position < dindex(oldest).position) oldest = j
              j-=1
            }
            oldest
          case x if x._1 > value => binarySearch_acc(start, i - 1, value)
          case x if x._2 < value => binarySearch_acc(i + 1, end, value)
        }
      }
      if(dindex(0).value == value) 0
      else if(dindex(dindex.length-1).value == value) {
        var newest = dindex.length-1
        var j = newest - 1
        while((j >= 0) && (dindex(j).value == value)) {
          if(dindex(j).position < dindex(newest).position) newest = j
          j-=1
        }
        newest
      }
      else binarySearch_acc( 0, dindex.length - 1, value)
    }

    //println(s"want to delete: $todelete, want to insert: $newPoint")
    val indextodelete = binarySearch_delete(0, dindex.length-1, todelete) // will always be pointing to an object
    val indextoinsert = binarySearch_insert(0, dindex.length-1, newPoint) // will always be pointing to an object or between two, in that case, the one after.
    //println(s"todelete: $indextodelete, toinsert: $indextoinsert")

    if((indextoinsert == indextodelete) | (indextoinsert == (indextodelete+1))) { // in that case it simply replaces the point
      dindex(indextodelete) = new T_Rank(dindex.length + offset, newPoint)
    } else {
      if(indextoinsert == dindex.length) { // necessarily, indextoinsert > indextodelete, and indextoinsert is not matching
        for (x <- indextodelete until indextoinsert-1) {
          dindex(x) = dindex(x + 1)
        }
        dindex(indextoinsert - 1) = new T_Rank(dindex.length + offset, newPoint)
      } else {
        val actualindextoinsert = if(dindex(indextoinsert).value == newPoint) indextoinsert +1 else indextoinsert

        if (actualindextoinsert < indextodelete) {
          for (x <- (actualindextoinsert+1 to indextodelete).reverse) {
            dindex(x) = dindex(x - 1)
          }
          dindex(actualindextoinsert) = new T_Rank(dindex.length + offset, newPoint)
        } else if (actualindextoinsert > indextodelete) {
          for (x <- indextodelete until actualindextoinsert-1) {
            dindex(x) = dindex(x + 1)
          }
          dindex(actualindextoinsert - 1) = new T_Rank(dindex.length + offset, newPoint)
        }
      }
    }
    offset += 1
    queue += newPoint
  }
}
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

import io.github.edouardfouche.index.tuple.CorrectedRankStreamTupleIndex

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Compute an adjusted, corrected rank index from a given data set
  * The rank are adjusted, which means that in the case of ties, the rank is defined as the average rank of the tying values
  * Also, a "correction for ties" is computed, as required to compute a Mann-Whitney U test
  * The correction is computed as a cumulative value.
  *
  * @param values A row-oriented data set
  */
class DimensionIndex_CorrectedRankStream(val values: Array[Double]) extends DimensionIndex  {
  type T = CorrectedRankStreamTupleIndex
  //def apply[U](implicit ord: Ordering[U]) = new DimensionIndex_CorrectedRank[U]

  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double]()
  var offset: Int = 0

  var dindex: Array[T] = createDimensionIndex(values)

  def refresh: Unit = {
    if(offset > 0) {
      dindex = dindex.zipWithIndex.map(x => new CorrectedRankStreamTupleIndex(x._1.position-offset, x._1.value, x._2, x._1.value))

      val m = dindex.length - 1
      var j = 0
      var acc_corr = 0.0
      while (j <= m) {
        var k = j
        var acc = 0.0
        // && is quite important here, as if the first condition is false you don't want to evaluate the second
        while ((k < m) && (dindex(k).value == dindex(k + 1).value)) { // Wooo we are comparing doubles here, is that ok? I guess yes
          acc += dindex(k).adjustedrank
          k += 1
        }
        if (k > j) {
          val newval = ((acc + dindex(k).adjustedrank) / (k - j + 1.0)).toFloat
          val t = k - j + 1.0
          acc_corr = acc_corr + math.pow(t , 3) - t
          (j to k).foreach(y => dindex(y) = CorrectedRankStreamTupleIndex(dindex(y).position, dindex(y).value, newval, acc_corr))
          j += k - j + 1 // jump to after the replacement
        } else {
          dindex(j) = CorrectedRankStreamTupleIndex(dindex(j).position, dindex(j).value, dindex(j).adjustedrank, acc_corr)
          j += 1
        }
      }
    }
    offset = 0
  }

  // TODO
  def insert(newpoint: Double): Unit = {
    val todelete = queue.dequeue

    // the binary search returns the index on the match with oldest position
    // if no match, the index just after
    def binarySearch_insert(start: Int, end: Int, value: Double): Int = {
      @tailrec
      def binarySearch_acc(start: Int, end: Int, value: Double): Int = { // this binary search is good only for finding the point where to insert
        val i = (end+start) / 2
        //println(s"start: $start, end: $end, i:$i")
        (dindex(i).value, dindex(i + 1).value) match {
          case x if (x._1 < value) & (x._2 >= value) => {
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
          case x if(x._1 == value) => {
            var oldest = i
            var j = oldest - 1
            while((j >= 0) && (dindex(j).value == value)) {
              if(dindex(j).position < dindex(oldest).position) oldest = j
              j-=1
            }
            oldest
          }
          case x if(x._2 == value) => {
            var oldest = i+1
            var j = oldest - 1
            while((j >= 0) && (dindex(j).value == value)) {
              if(dindex(j).position < dindex(oldest).position) oldest = j
              j-=1
            }
            oldest
          }
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

    println(s"want to delete: $todelete, want to insert: $newpoint")
    val indextodelete = binarySearch_delete(0, dindex.length-1, todelete) // will always be pointing to an object
    val indextoinsert = binarySearch_insert(0, dindex.length-1, newpoint) // will always be pointing to an object or between two, in that case, the one after.
    println(s"todelete: $indextodelete, toinsert: $indextoinsert")

    if((indextoinsert == indextodelete) | (indextoinsert == (indextodelete+1))) { // in that case it simply replaces the point
      dindex(indextodelete) = new CorrectedRankStreamTupleIndex(dindex.length + offset, newpoint, -1, -1)
    } else {
      if(indextoinsert == dindex.length) { // necessarily, indextoinsert > indextodelete, and indextoinsert is not matching
        for (x <- indextodelete until indextoinsert-1) {
          dindex(x) = dindex(x + 1)
        }
        dindex(indextoinsert-1) = new CorrectedRankStreamTupleIndex(dindex.length + offset, newpoint, -1, -1)
      } else {
        val actualindextoinsert = if(dindex(indextoinsert).value == newpoint) indextoinsert +1 else indextoinsert

        if (actualindextoinsert < indextodelete) {
          for (x <- (actualindextoinsert+1 to indextodelete).reverse) {
            dindex(x) = dindex(x - 1)
          }
          dindex(actualindextoinsert) =  new CorrectedRankStreamTupleIndex(dindex.length + offset, newpoint, -1, -1)
        } else if (actualindextoinsert > indextodelete) {
          for (x <- indextodelete until actualindextoinsert-1) {
            dindex(x) = dindex(x + 1)
          }
          dindex(actualindextoinsert-1) = new CorrectedRankStreamTupleIndex(dindex.length + offset, newpoint, -1, -1)
        }
      }
    }
    offset += 1
    queue += newpoint
  }

  def insertreplace(newdata: Array[Double]): Unit = {}

  def createDimensionIndex(input: Array[Double]): Array[T]= {
    input.foreach(x => queue += x)
    // Create an index for each column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = {
      input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._1._1, y._2.toFloat, y._1._1))
      /*
      try{
        input.map(_.toInt).zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
      } catch {
        case _: Throwable  => try{
          input.map(_.toDouble).zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
        } catch {
          case _: Throwable  => input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))
        }
      }

       */
    }
    val adjusted = new Array[CorrectedRankStreamTupleIndex](input.length)

    val m = nonadjusted.length - 1
    var j = 0
    var acc_corr = 0.0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._4 == nonadjusted(k + 1)._4)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._3
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._3) / (k - j + 1.0)).toFloat
        val t = k - j + 1.0
        acc_corr = acc_corr + math.pow(t , 3) - t
        (j to k).foreach(y => adjusted(y) = CorrectedRankStreamTupleIndex(nonadjusted(y)._1, nonadjusted(y)._2, newval, acc_corr))
        j += k - j + 1 // jump to after the replacement
      } else {
        adjusted(j) = CorrectedRankStreamTupleIndex(nonadjusted(j)._1, nonadjusted(j)._2, nonadjusted(j)._3, acc_corr)
        j += 1
      }
    }

    adjusted
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
}

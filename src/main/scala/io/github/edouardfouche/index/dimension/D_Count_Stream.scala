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

import scala.collection.mutable

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_Count_Stream(initvalues: Array[Double]) extends D_Count(initvalues) with DimensionIndexStream {
  override val id = "CountStream"
  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double](initvalues: _*)
  var offset = 0

  //override var dindex: mutable.Map[Double, T] = createDimensionIndex(values)

  override def refresh: Unit = {
    if(offset > 0) {
      //if(dindex.keys.toArray.exists(x=> x.isNaN)) {
      //  println(dindex.keys.mkString(","))
      //}
      //dindex.keys.foreach(x => dindex(x) = T_Count(dindex(x)._1.map(y => y - offset), dindex(x)._2))
      dindex = dindex.map(x => (x._1, (x._2._1.map(y => y - offset), x._2._2)))
      offset = 0
    }

  }

  override def insert(newpoint: Double): Unit = {
    currentvalues = currentvalues.drop(1) :+ newpoint // be careful: required for CSP
    val todelete = queue.dequeue()
    // handle insertion

    val current = dindex.getOrElseUpdate(newpoint, (Vector[Int](), 0))
    //dindex(newpoint) = (current._1.enqueue(values.length + offset),current._2 +1)
    dindex(newpoint) = (current._1 :+ (currentvalues.length + offset), current._2 + 1)

    //if (dindex.getOrElse(newpoint, -1) != -1) { // in that case we already have an entry for this category
    //  dindex(newpoint) = T_Count(dindex(newpoint)._1.enqueue(values.length + offset), dindex(newpoint)._2 + 1)
    //} else {
    //  // Handle the case were this is a new category
    //  dindex(newpoint) = T_Count(scala.collection.immutable.Queue[Int](values.length + offset), 1)
    //}
    // handle deletion
    if (dindex(todelete)._2 > 1) { // In that case we don't need to remove the entry
      //val position = dindex(todelete)._1.zipWithIndex.min._2
      //dindex(todelete) = (dindex(todelete)._1.dequeue._2, dindex(todelete)._2 - 1)
      dindex(todelete) = (dindex(todelete)._1.tail, dindex(todelete)._2 - 1)
    } else {
      dindex.remove(todelete)
      // For those guys, we need to find where it is.
      //val postodelete = categories.zipWithIndex.filter(_._1 == todelete)(0)._2
      //categories = categories.take(postodelete) ++ categories.drop(postodelete+1)
      //dindex = dindex.take(postodelete) ++ dindex.drop(postodelete+1)
    }
    offset += 1
    queue += newpoint
    //if(dindex.keys.toArray.exists(x=> x.isNaN)) {
    //  println(s"todelete: $todelete, newpoint: $newpoint, keys:${dindex.keys.mkString(",")}")
    //}
  }
}

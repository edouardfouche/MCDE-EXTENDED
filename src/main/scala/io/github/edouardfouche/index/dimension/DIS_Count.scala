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
  * @param values An array of values corresponding to the values in a column
  */
class DIS_Count(values: Array[Double]) extends DI_Count(values) with DimensionIndexStream {
  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double](values: _*)
  var offset = 0

  //var dindex: Array[T] = createDimensionIndex(values)

  override def refresh: Unit = {
    if(offset > 0) {
      dindex(0).map.keys.foreach(x => dindex(0).map(x) = (dindex(0).map(x)._1.map(y => y-offset), dindex(0).map(x)._2))
    }
    offset = 0
  }

  override def insert(newpoint: Double): Unit = {
    val todelete = queue.dequeue()
    // handle insertion
    if(dindex(0).map.getOrElse(newpoint, -1) != -1) { // in that case we already have an entry for this category
      dindex(0).map(newpoint) = (dindex(0).map(newpoint)._1 :+ values.length+offset, dindex(0).map(newpoint)._2 + 1)
    } else {
      // Handle the case were this is a new category
      dindex(0).map(newpoint) = (Array(values.length+offset), 1)
    }
    // handle deletion
    if(dindex(0).map(todelete)._2 > 1) { // In that case we don't need to remove the entry
      val position = dindex(0).map(todelete)._1.zipWithIndex.min._2
      dindex(0).map(todelete) = (dindex(0).map(todelete)._1.take(position) ++ dindex(0).map(todelete)._1.drop(position+1),
        dindex(0).map(todelete)._2 -1)
    } else {
      dindex(0).map.remove(todelete)
      // For those guys, we need to find where it is.
      //val postodelete = categories.zipWithIndex.filter(_._1 == todelete)(0)._2
      //categories = categories.take(postodelete) ++ categories.drop(postodelete+1)
      //dindex = dindex.take(postodelete) ++ dindex.drop(postodelete+1)
    }
    offset += 1
    queue += newpoint
  }
}

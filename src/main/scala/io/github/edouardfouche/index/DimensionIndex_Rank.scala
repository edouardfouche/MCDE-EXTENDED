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

import io.github.edouardfouche.index.tuple.{RankTupleIndex, TupleIndex}
import io.github.edouardfouche.preprocess.Preprocess

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  * @param values
  */
class DimensionIndex_Rank(val values: Vector[String]) extends DimensionIndex {
  type T = RankTupleIndex

  var dindex: Array[T] = createDimensionIndex(values)

  // TODO
  def insert(newdata: Vector[U]): Unit = {}
  def insertreplace(newdata: Vector[U]): Unit = {}

  def createDimensionIndex(input: Vector[U]): Array[T] = {
    try{
      input.map(_.toInt).zipWithIndex.sortBy(_._1).map(x => RankTupleIndex(x._2)).toArray
    } catch {
      case _: Throwable => try{
        input.map(_.toDouble).zipWithIndex.sortBy(_._1).map(x => RankTupleIndex(x._2)).toArray
      } catch {
        case _: Throwable  => input.zipWithIndex.sortBy(_._1).map(x => RankTupleIndex(x._2)).toArray
      }
    }
  }
}

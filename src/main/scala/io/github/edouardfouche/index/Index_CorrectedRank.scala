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

// Here the inputs may be row-oriented
// This is good but restricted to the same time for each Array
class Index_CorrectedRank(val data: DataSet, val parallelize: Int = 0) extends Index[DimensionIndex_CorrectedRank] {
  //type T = DimensionIndex_CorrectedRank[String]

  protected def createIndex(data: DataSet): Vector[DimensionIndex_CorrectedRank] = {
    (0 until data.ncols).toVector.map(data(_)).map {
      //case x: Vector[Double] => new DimensionIndex_CorrectedRank[Double](x)
      //case x: Vector[Int] => new DimensionIndex_CorrectedRank[Int](x)
      case x: Vector[String] => new DimensionIndex_CorrectedRank(x)
      case x => throw new Error(s"Unsupported type of {${x mkString ","}}")
    }
  }

  /*
  def f(data: Array[Array[_]]): Array[Array[_ >: Double with Int with String]] = {
    data.map {
      case x: Array[Double] => x
      case x: Array[Int] => x
      case x: Array[String] => x
      case _ => throw new Error(s"Unsupported type")
    }
  }
  */

}

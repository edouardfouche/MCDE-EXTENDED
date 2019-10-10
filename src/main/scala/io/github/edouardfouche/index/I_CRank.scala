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

import io.github.edouardfouche.index.dimension.D_CRank
import io.github.edouardfouche.preprocess.DataSet

import scala.collection.parallel.ForkJoinTaskSupport

// Here the inputs may be row-oriented
// This is good but restricted to the same time for each Array
class I_CRank(val data: DataSet, val parallelize: Int = 0) extends Index[D_CRank] {
  //override type T = D_CRank
  val id = "CRank"
  //type T = D_CRank[String]

  protected def createIndex(data: DataSet): Vector[D_CRank] = {
    if (parallelize == 0) {
      (0 until data.ncols).toVector.map(data(_)).map {
        case x: Array[Double] => new D_CRank(x)
        case x => throw new Error(s"Unsupported type of {${x mkString ","}}")
      }
    } else {
      val columns = (0 until data.ncols).par
      if (parallelize > 1) columns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      columns.toVector.map(data(_)).map {
        case x: Array[Double] => new D_CRank(x)
        case x => throw new Error(s"Unsupported type of {${x mkString ","}}")
      }
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

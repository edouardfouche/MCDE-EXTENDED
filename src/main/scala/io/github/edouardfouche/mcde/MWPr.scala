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
package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.dimension.D_Rank
import io.github.edouardfouche.index.{I_Rank, I_Rank_Stream}
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.HalfGaussian

import scala.annotation.tailrec

/**
  * Simply like MWP but not adjusting and correcting for ties
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  */
case class MWPr(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats{
  type I = I_Rank
  type D = D_Rank
  val id = "MWPr"

  override def getDIndexConstruct: Array[Double] => D = new D(_)

  override def getIndexConstruct: DataSet => I = new I(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream)
      new I(input, parallelize)
    else new I_Rank_Stream(input, parallelize)
  }

  /**
    * Compute a statistical test based on  Mann-Whitney U test
    *
    * @param ref            The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The Mann-Whitney statistic
    */
  def twoSample(ref: D, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")

    val sliceStart = scala.util.Random.nextInt((indexSelection.length * (1-beta)).toInt+1)
    val sliceEnd = sliceStart + (indexSelection.length * beta).toInt//.min(indexSelection.length - 1)

    //println(s"sliceStart: $sliceStart, sliceEnd: $sliceEnd, reference: $reference")

    def getStat(cutStart: Int, cutEnd: Int): Double = {
      @tailrec def cumulative(n: Int, acc: Double, count: Long): (Double, Long) = {
        if (n == cutEnd) (acc - (cutStart * count), count) // correct the accumulator in case the cut does not start at 0
        else if (indexSelection(ref(n)._1)) cumulative(n + 1, acc + n, count + 1)
        else cumulative(n + 1, acc, count)
      }

      lazy val cutLength = cutEnd - cutStart
      val (r1, n1:Long) = cumulative(cutStart, 0, 0)
      //println(s"indexCount: ${indexSelection.count(_ == true)}, sliceStart: ${sliceStart}, sliceEnd: ${sliceEnd}, r1: $r1, n1: $n1, cutLength: $cutLength")
      val P = if (n1 == 0 | n1 == cutLength) { // in the case it is empty or contains them all, the value should be maximal
        1
      }
      else {
        val n2:Long = cutLength - n1
        if(n1 >= 3037000499L && n2 >= 3037000499L) throw new Exception("Long type overflowed. Dataset has to many dataobjects. Please subsample and try again with smaller dataset.")
        val U1 = r1 - (n1 * (n1 - 1)) / 2 // -1 because our ranking starts from 0
        val std = math.sqrt((n1 * n2 * (cutLength + 1.0)) / 12.0) // without correction
        // note: n1 + n2 = n1 + cutLength - n1 = cutLength
        val mean = (n1 * n2) / 2.0
        val Z = math.abs((U1 - mean) / std)
        val res = HalfGaussian.cdf(Z) // * (cutLength.toFloat / reference.length)
        //println(s"Z: ${Z}, P: $res,  sliceStart: ${sliceStart}, sliceEnd: ${sliceEnd}, r1: $r1, n1: $n1, cutLength: $cutLength")
        res
      }
      P
    }

    getStat(sliceStart, sliceEnd)
  }
}

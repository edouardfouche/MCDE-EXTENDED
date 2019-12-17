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

import scala.annotation.tailrec
import scala.math.pow

/**
  * This is a re-implementation  of the contrast measure as proposed in HiCS, that is fitting the MCDE framework
  * Use the Kolmogorov-Smirnov test as basis. Compute the exact p-values (whenever there are no ties)
  * Use marginal restriction
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  */
case class KSPemr(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0, stream: Boolean = false) extends McdeStats {
  type I = I_Rank
  type D = D_Rank
  val id = "KSPemr"

  override def getDIndexConstruct: Array[Double] => D = new D(_)

  override def getIndexConstruct: DataSet => I = new I(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream) new I(input, parallelize)
    else new I_Rank_Stream(input, parallelize)
  }

  /**
    * Compute the Kolmogorov Smirnov test
    *
    * @param ref            The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  def twoSample(ref: D, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")
    val sliceStart = scala.util.Random.nextInt((indexSelection.length * (1 - beta)).toInt + 1)
    val sliceEnd = sliceStart + (indexSelection.length * beta).toInt //.min(indexSelection.length - 1)

    val theref = (sliceStart until sliceEnd).map(x => indexSelection(ref(x)._1))
    val inSlize = theref.count(_ == true)
    val outSlize = theref.length - inSlize


    if (inSlize == 0 || outSlize == 0) return 1.0 // If one is empty they are perfectly different --> score = 1 (and no prob with division by 0)
    val selectIncrement = 1.0 / inSlize
    val refIncrement = 1.0 / outSlize

    @tailrec def cumulative(n: Int, acc1: Double, acc2: Double, currentMax: Double): Double = {
      if (n == theref.length) currentMax
      else {
        if (theref(n))
          cumulative(n + 1, acc1 + selectIncrement, acc2, currentMax max math.abs(acc2 - (acc1 + selectIncrement)))
        else
          cumulative(n + 1, acc1, acc2 + refIncrement, currentMax max math.abs(acc2 + refIncrement - acc1))
      }
    }

    val D = cumulative(0, 0, 0, 0)
    val p = get_p_from_D(D, inSlize, outSlize)
    //println(s"ref.length: ${ref.length}, start: ${sliceStart}, end: ${sliceEnd}, inSlice: $inSlize, outSLice: $outSlize, inc1: ${selectIncrement}, inc2: $refIncrement, D: $D")
    p
  }

  /**
    * Convert the D value into a p-value
    * Note: This function is basically a transcription from psmirnov2x (standart R source in C)
    *
    * @param D  D value from KSPmr test
    * @param n1 n Datapoints in first sample
    * @param n2 n Datapoints in second sample
    * @return p-value of two-sided two-sample KSPmr
    */
  def get_p_from_D(D: Double, n1: Long, n2: Long): Double = {

    val (m, n) = if (n1 > n2) (n2, n1) else (n1, n2)
    val md = m.toDouble
    val nd = n.toDouble
    /*
    q has 0.5/mn added to ensure that rounding error doesn't
    turn an equality into an inequality, eg abs(1/2-4/5)>3/10
    */
    val q = (0.5 + math.floor(D * md * nd - pow(10, -7))) / (md * nd)
    //val u = new Array[Double](n.toInt+1)
    //u.indices.foreach(j => if((j /nd)> q) u(j) = 0 else u(j) = 1)
    val u = (0 to n.toInt).map(j => if ((j / nd) > q) 0.0 else 1.0).toArray

    for (i <- 1 to m.toInt) {
      val w = i.toDouble / (i + n).toDouble
      if ((i / md) > q) {
        u(0) = 0
      } else {
        u(0) = w * u(0)
      }
      (1 to n.toInt).foreach { j =>
        if (math.abs(i / md - j / nd) > q) {
          u(j) = 0
        } else {
          u(j) = w * u(j) + u(j - 1)
        }
      }
    }
    //println(s"n1 : $n1, n2 : $n2 -> ${u(n.toInt)}")
    u(n.toInt)
  }
}

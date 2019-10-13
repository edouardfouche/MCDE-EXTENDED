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

import io.github.edouardfouche.index.dimension.D_CRank
import io.github.edouardfouche.index.{I_CRank, I_CRank_Stream}
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.HalfGaussian

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Compute the average accross the p-values of all the slices, but this time do the tie correction
  * The tie correction is precomputed as a Map, which gives for each distinct rank a corresponding correction
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  *              Added with respect to the original paper to loose the dependence of beta from alpha.
  */

case class MWPu(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5,
                var parallelize: Int = 0) extends McdeStats {
  //type PreprocessedData = D_CRank
  //type U = Double
  type I = I_CRank
  type D = D_CRank
  val id = "MWPu"

  override def getDIndexConstruct: Array[Double] => D = new D(_)

  override def getIndexConstruct: DataSet => I = new I(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream)
      new I(input, parallelize)
    else new I_CRank_Stream(input, parallelize)
  }

  /**
    * Compute the contrast of a subspace
    *
    * @param m          The indexes from the original data ordered by the rank of the points
    * @param dimensions The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @return The contrast of the subspace (value between 0 and 1)
    */
  override def contrast(m: I, dimensions: Set[Int]): Double = {
    // Sanity check
    m.refresh()
    require(dimensions.forall(x => x >= 0 & x < m.ncols), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = if (parallelize == 0) {
      (1 to M).map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.uniformSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    } else {
      val iterations = (1 to M).par
      if (parallelize > 1) {
        //iterations.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.uniformSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    }

    //if(calibrate) Calibrator.calibrateValue(result, StatsFactory.getTest(this.id, this.M, this.alpha, calibrate=false), dimensions.size, m(0).length)// calibrateValue(result, dimensions.size, alpha, M)
    //else result
    result
  }

  /**
    * Compute a statistical test based on  Mann-Whitney U test using a reference vector (the indices of a dimension
    * ordered by the rank) and a set of Int that correspond to the intersection of the position of the element in the
    * slices in the other dimensions.
    *
    * @param ref            The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The Mann-Whitney statistic
    */
  def twoSample(ref: D, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")
    //val start = scala.util.Random.nextInt((indexSelection.length * (1-beta)).toInt)//+1)
    //val start = scala.util.Random.nextInt(ref.length) //+1)
    //val sliceStart = ref.getSafeCut(start) %ref.length
    //val sliceEndSearchStart = (sliceStart + (indexSelection.length * beta).toInt) % ref.length
    //val sliceEnd = ref.getSafeCut(sliceEndSearchStart) %ref.length

    val sliceStart = 0
    val sliceEnd = ref.length

    // Correcting the marginal restriction (?)
    //val (sliceStart: Int, sliceEnd: Int) = if (ref(safeSliceStart)._2 == ref(safeSliceEnd - 1)._2) {
    //  val flag = math.random() < 0.5
    //  if (flag) if(safeSliceStart == 0) (ref.getSafeCutLeft(ref.length-1), safeSliceEnd) else (ref.getSafeCutLeft(safeSliceStart - 1), safeSliceEnd)
    //  else (safeSliceStart, ref.getSafeCutRight(safeSliceEnd % ref.length))
    //  //else (safeSliceStart, safeSliceEnd)
    //} else (safeSliceStart, safeSliceEnd)

    /*
    try {
      val (sliceStart: Int, sliceEnd: Int) = if(ref(safeSliceStart)._2 == ref(safeSliceEnd-1)._2) {
        if(safeSliceStart > 0) (ref.getSafeCutLeft(safeSliceStart - 1), safeSliceEnd)
        else if(safeSliceEnd < ref.length) (safeSliceStart, ref.getSafeCutRight(safeSliceEnd))
        else (safeSliceStart, safeSliceEnd)
      } else (safeSliceStart, safeSliceEnd)
    } catch {
      case e: Throwable => {
        println(s"ref.length: ${ref.length}, start = $start, safeStart = $safeSliceStart, safeEnd = $safeSliceEnd")
        throw e
      }
    }
    */


    //println(s"ref.length: ${ref.length}, start = $start, safeStart = $safeSliceStart, safeEnd = $safeSliceEnd, sliceStart = $sliceStart, sliceEnd = $sliceEnd")
    //println(s"indexSelection.length: ${indexSelection.length}, start: $start, actualStart: $sliceStart, sliceEnd: $sliceEnd, reference: $reference")

    //val ref: D_CRank[String] = index(reference)

    def getStat(cutStart: Int, cutEnd: Int): Double = {
      @tailrec def cumulative(n: Int, acc: Double, count: Long): (Double, Long) = {
        if (n == cutEnd) (acc, count) // correct the accumulator in case the cut does not start at 0
        else if (indexSelection(ref(n)._1)) cumulative(n + 1, acc + ref(n)._3, count + 1)
        else cumulative(n + 1, acc, count)
      }

      lazy val cutLength = if(cutEnd > cutStart) cutEnd - cutStart else (ref.length - cutStart + cutEnd-1)
      val (r1, n1: Long) = cumulative(cutStart, 0, 0)

      //if (n1 == 0){ //| n1 == cutLength) {
      //  1 // If the inslice is empty, this just means maximal possible score.
      //} else if (n1 == cutLength) {
      //  0 // If the outslice is empty, then the process led to selecting all dimensions, and contrast is meaningless
      //}

      //if (n1 == indexSelection.length) { // In that case the slicing process led to selecting everything, contrast is not defined
      //  0
      //} else
      if (n1 == 0 || n1 == cutLength) {
        1
      } else {
        val n2: Long = cutLength - n1
        if (n1 >= 3037000499L && n2 >= 3037000499L) throw new Exception("Long type overflowed. Too many objects: Please subsample and try again with smaller data set.")
        val U1 = r1 - (n1 * (n1 - 1)) / 2 // -1 because our ranking starts from 0

        //val corr = if((cutEnd == 0) & (cutStart == 0)) ref(ref.length-1)._4
        //else if(cutEnd == 0) ref(ref.length-1)._4 - ref(cutStart - 1)._4
        //else if(cutStart == 0) ref(cutEnd - 1)._4
        //else if(cutEnd > cutStart) ref(cutEnd - 1)._4 - ref(cutStart - 1)._4
        //else ref(cutEnd - 1)._4 + ref(ref.length-1)._4 - ref(cutStart - 1)._4

        val corr = ref(cutEnd - 1)._4

        //val corrMax = if (cut End == 0) ref(ref.length-1)._4 else ref(cutEnd - 1)._4
        //val corrMin = if (cutStart == 0) 0.0 else ref(cutStart - 1)._4

        val correction = corr / (cutLength.toDouble * (cutLength.toDouble - 1.0))
        val std = math.sqrt((n1.toDouble * n2.toDouble / 12.0) * (cutLength.toDouble + 1.0 - correction)) // handle ties https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test

        if (std == 0) 0 // This happens in the extreme case that the cut consists in only one unique value
        else {
          val mean = (n1.toDouble * n2.toDouble) / 2.0
          val Z = math.abs((U1 - mean) / std)
          val res = HalfGaussian.cdf(Z)
          if (res.isNaN) {
            print(s"reference: ${ref.dindex.slice(cutStart, cutEnd).take(5) mkString ","}")
            println(s"U1: $U1, U2: ${n1 * n2 - U1}, n1: $n1, n2: $n2, std: $std, correction: $correction -> res: $res")
          }
          res
        }
      }
    }

    val res = getStat(sliceStart, sliceEnd)
    //println(s"res : $res")
    res
  }
}
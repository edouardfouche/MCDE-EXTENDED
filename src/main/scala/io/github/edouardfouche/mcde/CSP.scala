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

import io.github.edouardfouche.index.{DimensionIndex_Count, Index, Index_Count}
import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.{E, pow, sqrt}
import breeze.stats.distributions.ChiSquared

/**
  * Chi-Squared test whithin the MCDE framework
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta Expected share of instances in marginal restriction (reference dimension).
  *       Added with respect to the original paper to loose the dependence of beta from alpha.
  *
  */
//TODO: It would be actually interesting to compare MCDE with a version with the KSP-test AND all the improvements proposed by MCDE
case class CSP(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  //type U = Double
  //type PreprocessedData = DimensionIndex_Rank
  type I = Index_Count
  val id = "CSP"

  //TODO: We are not handling the marginal restriction for the moment (beta)

  def preprocess(input: DataSet): Index_Count = {
    new Index_Count(input, 0) //TODO: seems that giving parallelize another value that 0 leads to slower execution, why?
  }

  /**
    * Overriding because the slice size is different in that case
    * Compute the contrast of a subspace
    *
    * @param m          The indexes from the original data ordered by the rank of the points
    * @param dimensions The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @return The contrast of the subspace (value between 0 and 1)
    */
  override def contrast(m: I, dimensions: Set[Int]): Double = {
    // Sanity check
    //require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    //val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = if (parallelize == 0) {
      (1 to M).map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        // There should be at least 1 and at most number of categories-1
        val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.index(referenceDim).dindex.length).ceil.toInt.min(m.index(referenceDim).dindex.length-1).max(1)
        //println(s"slicesize: $sliceSize")
        twoSample(m, referenceDim, m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    } else {
      val iterations = (1 to M).par
      if (parallelize > 1) {
        //iterations.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        iterations.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        // There should be at least 1 and at most number of categories-1
        val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.index(referenceDim).dindex.length).ceil.toInt.min(m.index(referenceDim).dindex.length-1).max(1)
        //println(s"slicesize: $sliceSize")
        twoSample(m, referenceDim, m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    }

    //if(calibrate) Calibrator.calibrateValue(result, StatsFactory.getTest(this.id, this.M, this.alpha, calibrate=false), dimensions.size, m(0).length)// calibrateValue(result, dimensions.size, alpha, M)
    //else result
    result
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    * @param reference      The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  def twoSample(index: Index_Count, reference: Int, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")

    val ref: DimensionIndex_Count[String] = index(reference)

    val selectedvalues: Array[String] = indexSelection.zipWithIndex.filter(_._1 == true).map(x => ref.values(x._2))

    if(selectedvalues.length == 0) 1.0 // Nothing in the slide. Maximal value then
    else{
      val selectedcounts: Map[String, Int] = selectedvalues.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.length)})

      // now let's compare the selectedcounts with the ref.counts according to chisq

      val expectedcounts: Map[String, Double]  = ref.counts.map({case (x,y) => (x, (y.toDouble / ref.values.length.toDouble)*(ref.values.length.toDouble*alpha))})

      val statistics = ref.categories.map(stat => {
        val observed = selectedcounts.getOrElse(stat,0).toDouble
        val expected = (ref.counts(stat).toDouble / ref.values.length.toDouble)*selectedvalues.length.toDouble
        math.pow((observed - expectedcounts(stat)),2)/expectedcounts(stat)
      })
      val teststatistics = statistics.sum

      val ndegree = ref.categories.length - 1

      val chsq = ChiSquared(ndegree).cdf(teststatistics)
      //println(s"ref: $reference, ndegree: $ndegree, npoint: ${ref.values.length}, nselected: ${selectedvalues.length}  stat: $teststatistics, chsq: $chsq")
      //println(selectedcounts.toString())
      //println(expectedcounts.toString)
      chsq
    }

  }
}

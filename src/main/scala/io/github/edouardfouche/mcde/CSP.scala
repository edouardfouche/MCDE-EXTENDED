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
  type D = DimensionIndex_Count
  type I = Index_Count

  val id = "CSP"

  //TODO: How is the handling of marginal restriction?
  //TODO: It does not really seems uniform in the independent case.

  def preprocess(input: DataSet): Index_Count = {
    new Index_Count(input, 0) //TODO: seems that giving parallelize another value that 0 leads to slower execution, why?
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    *
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
    // @param reference      The original position of the elements of a reference dimension ordered by their rank
  def twoSample(ref: DimensionIndex_Count, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")

    //val ref: DimensionIndex_Count[String] = index(reference)

    // the marginal restriction
    /*
      val shuffledCategories: List[_] = scala.util.Random.shuffle(ref.categories.toList)//.take(sliceSize)
    @scala.annotation.tailrec
    def cumulative(current: Int, categories: List[_], selectedCategories: List[Int]): List[Int] = {
      if(current < ref.values.length*beta && categories.nonEmpty) {
        cumulative(current + ref.counts(categories.head), categories.tail, selectedCategories :+ categories.head)
      } else selectedCategories
    }
    val restrictedCategories: Set[_] = cumulative(0, shuffledCategories, List[Int]()).map(x => ref.int_to_value(x)).toSet
    */

    val restrictedCategories: Array[Double] = ref.selectCategories(math.ceil(ref.values.length*beta).toInt)
    //val restrictedCategories: List[String] = ref.categories.toList
    val restrictedselection = indexSelection.zipWithIndex.map(x => (x._1,ref.values(x._2))).filter(x => restrictedCategories.contains(x._2))
    val sample1 = restrictedselection.filter(_._1 == true).map(_._2)
    val sample2 = restrictedselection.filter(_._1 == false).map(_._2)

    //val nonrestrictedvalues: Array[Double] = indexSelection.zipWithIndex.filter(_._1 == true).map(x => ref.values(x._2))
    //val selectedvalues = nonrestrictedvalues.filter(x => restrictedCategories.contains(x))
    //val selectedvalues = nonrestrictedvalues

    if((sample1.length == 0) | (sample2.length == 0)) 1.0 // Nothing in the slide. Maximal value then
    else{
      // count the occurences of each categories in the selection
      // val selectedcounts: Map[Double, Int] = selectedvalues.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.length)})


      val sample1counts: Map[Double, Int] = sample1.groupBy(identity).mapValues(_.length)
      //val sample2counts: Map[Double, Int] = sample2.groupBy(identity).mapValues(_.length) // could we speed this up?
      val sample2counts: Map[Double, Int] = restrictedCategories.map(x => x -> (ref.dindex(0).toMap(x)._2 - sample1counts.getOrElse(x,0))).toMap

      // now let's compare the selectedcounts with the ref.counts according to chisq

      // ref.values.length.toDouble*alpha


      // Here this is wrong to take ref.values.length.toDouble instead:
      // restrictedCategories.map(x => ref.counts(x)).sum.toDouble
      //val total = restrictedCategories.map(x => ref.counts(x)).sum.toDouble
      // the length of the nonrestricted !

      /*
      val expectedcounts: Map[Double, Double]  = ref.counts.map({case (x,y) => (x, (y.toDouble / ref.values.length.toDouble)*(nonrestrictedvalues.length.toDouble))})
      val statistics = restrictedCategories.map(stat => {
        val observed = selectedcounts.getOrElse(stat,0).toDouble
        //val expected = (ref.counts(stat).toDouble / ref.values.length.toDouble)*selectedvalues.length.toDouble
        math.pow((observed - expectedcounts(stat)),2)/expectedcounts(stat)
      })
      val teststatistics = statistics.sum
      */

      val n1 = sample1.length.toDouble
      val n2 = sample2.length.toDouble
      val N = n1 + n2

      val statistics = restrictedCategories.map(stat => {
        val o1 = sample1counts.getOrElse(stat,0).toDouble
        val o2 = sample2counts.getOrElse(stat,0).toDouble
        val tot = o1 + o2

        val e1 = (tot * n1) /  N
        val e2 = (tot * n2) /  N

        math.pow((o1 - e1),2)/e1 + math.pow((o2 - e2),2)/e2
      })
      val teststatistics = statistics.sum

      //val ndegree = math.pow(restrictedCategories.size - 1,2)
      //TODO: In some extreme cases, ndegree becomes 0, (because restricting on only one categorie, in that case, this is a one-way chi-squared test
      val ndegree = (restrictedCategories.length - 1).max(1)
      //val ndegree = ref.categories.size - 1

      val chsq = ChiSquared(ndegree).cdf(teststatistics)

      //println(s"ndegree: $ndegree, sample1size: ${sample1.size}, sample2size: ${sample2.size}, stat: $teststatistics, chsq: $chsq, restrictedcats: ${restrictedCategories mkString ","}")
      //println(s"restrictedcats: ${restrictedCategories.toString}")
      //println(s"s: ${selectedcounts.toString}")
      //println(s"e: ${expectedcounts.toString}")
      //println(s"a: ${ref.counts.toString}")
      chsq
    }

  }
}
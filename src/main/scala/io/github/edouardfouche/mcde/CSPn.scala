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

import breeze.stats.distributions.ChiSquared
import io.github.edouardfouche.index.dimension.D_Count
import io.github.edouardfouche.index.{I_Count, I_Count_Stream}
import io.github.edouardfouche.preprocess.DataSet

/**
  * Chi-Squared test whithin the MCDE framework
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  *              Added with respect to the original paper to loose the dependence of beta from alpha.
  *              Like CSP but does not have marginal restriction (beta parameter is not used)
  *
  */
case class CSPn(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  //type U = Double
  //type PreprocessedData = D_Rank
  type D = D_Count
  type I = I_Count
  val id = "CSPn"

  override def getDIndexConstruct: Array[Double] => D = new D(_)

  override def getIndexConstruct: DataSet => I = new I(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream) new I(input, parallelize)
    else new I_Count_Stream(input, parallelize)
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    * @param ref            The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  def twoSample(ref: D, indexSelection: Array[Boolean]): Double = {

    /* // Marginal restriction
    val restrictedCategories: Array[Double] = ref.selectRestriction(math.ceil(ref.values.length*beta).toInt) // ref.dindex.keys.toArray
    if(restrictedCategories.length == 1) return 0 // In that case, contrast is undefined.
    val restrictedselection = indexSelection.zipWithIndex.map(x => (x._1,ref.values(x._2))).filter(x => restrictedCategories.contains(x._2))
    val sample1: Array[Double] = restrictedselection.filter(_._1 == true).map(_._2)
    val sample2: Array[Double] = restrictedselection.filter(_._1 == false).map(_._2)
    */

    //val nonrestrictedvalues: Array[Double] = indexSelection.zipWithIndex.filter(_._1 == true).map(x => ref.values(x._2))
    //val selectedvalues = nonrestrictedvalues.filter(x => restrictedCategories.contains(x))
    //val selectedvalues = nonrestrictedvalues
    if (ref.dindex.keys.size == 1) return 0 // In that case, contrast is undefined.
    val selection = indexSelection.zipWithIndex.map(x => (x._1, ref.currentvalues(x._2)))
    val sample1: Array[Double] = selection.filter(_._1 == true).map(_._2)
    val sample2: Array[Double] = selection.filter(_._1 == false).map(_._2)

    if ((sample1.length == 0) | (sample2.length == 0)) 1.0 // Nothing in the slide. Maximal value then
    else {
      // count the occurences of each categories in the selection
      // val selectedcounts: Map[Double, Int] = selectedvalues.zipWithIndex.groupBy(_._1).map({case (x,y) => (x,y.length)})


      val sample1counts: Map[Double, Int] = sample1.groupBy(identity).mapValues(_.length)
      //val sample2counts: Map[Double, Int] = sample2.groupBy(identity).mapValues(_.length) // could we speed this up?
      //val sample2counts: Map[Double, Int] = restrictedCategories.map(x => x -> (ref.dindex(x)._2 - sample1counts.getOrElse(x, 0))).toMap
      val sample2counts: Map[Double, Int] = ref.dindex.keys.map(x => x -> (ref.dindex(x)._2 - sample1counts.getOrElse(x, 0))).toMap

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

      val statistics = ref.dindex.keys.map(stat => {
        val o1 = sample1counts.getOrElse(stat, 0).toDouble
        val o2 = sample2counts.getOrElse(stat, 0).toDouble
        val tot = o1 + o2

        val e1 = (tot * n1) / N
        val e2 = (tot * n2) / N

        val s = math.pow(o1 - e1, 2) / e1 + math.pow(o2 - e2, 2) / e2
        //println(s"o1 : $o1, o2 : $o2, e1: $e1, e2: $e2, s: $s")
        s
      })
      val teststatistics = statistics.sum

      //val ndegree = math.pow(restrictedCategories.size - 1,2)
      //TODO: In some extreme cases, ndegree becomes 0, (because restricting on only one categorie, in that case, this is a one-way chi-squared test
      val ndegree = (ref.dindex.keys.size - 1).max(1)
      //val ndegree = ref.categories.size - 1

      val chsq = ChiSquared(ndegree).cdf(teststatistics)

      //println(s"ndegree: $ndegree, sample1size: ${sample1.size}, sample2size: ${sample2.size}, stat: $teststatistics, chsq: $chsq")
      //println(s"restrictedcats: ${restrictedCategories.toString}")

      //println(s"nrestr: ${restrictedCategories.length}, ([${restrictedCategories.mkString(",")}), n1: ${sample1.size}, n2: ${sample2.size} stat: $teststatistics, chsq: $chsq")
      //println(s"s1 : ${sample1counts}; s2 : ${sample2counts}")

      //println(s"s: ${selectedcounts.toString}")
      //println(s"e: ${expectedcounts.toString}")
      //println(s"a: ${ref.counts.toString}")
      chsq
    }

  }
}

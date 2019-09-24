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

import io.github.edouardfouche.index.I_Multi
import io.github.edouardfouche.index.dimension.{D_CRank, D_Count, D_Rank, DimensionIndex}
import io.github.edouardfouche.preprocess.DataSet

/**
  * Chi-Squared test whithin the MCDE framework
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta Expected share of instances in marginal restriction (reference dimension).
  *       Added with respect to the original paper to loose the dependence of beta from alpha.
  *
  */
//TODO: It would be actually interesting to compare MCDE with a version with the KSP-test AND all the improvements proposed by MCDE
case class AUTOP(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  //type U = Double
  //type PreprocessedData = D_Rank
  type D = DimensionIndex
  type I = I_Multi

  val id = "CSP"

  //TODO: How is the handling of marginal restriction?

  def preprocess(input: DataSet): I_Multi = {
    new I_Multi(input, 0) //TODO: seems that giving parallelize another value that 0 leads to slower execution, why?
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  def twoSample(ref: DimensionIndex, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")
    //ref.refresh // handled inside
    ref match {
      case x: D_Count =>
        //println(s"$ref : CSP!")
        CSP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case x: D_Rank =>
        //println(s"$ref : KSP!")
        KSP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case x: D_CRank =>
        //println(s"$ref : MWP!")
        MWP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case _ => throw new Error("Unsupported DimensionIndex type")
    }
  }
}

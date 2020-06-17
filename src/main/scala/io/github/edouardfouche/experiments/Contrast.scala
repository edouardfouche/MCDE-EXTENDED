/*
 * Copyright (C) 2020 Edouard Fouché
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
package io.github.edouardfouche.experiments

import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._

/**
  * Created by fouchee on 12.07.17.
  * Check the distribution of contrast of various estimators
  */
object Contrast extends Experiment {
  val nrep = 10000 // reduce if too large
  def run(): Unit = {
    info(s"Starting com.edouardfouche.experiments")

    val tests = Vector(
      MWP(1, 0.5, 0.5),
      MWPnomr(1, 0.5, 0.5),
      MWPu(1, 0.5, 0.5),
      //MWPr(1,0.5, 0.5),
      KSPemr(1, 0.5, 0.5),
      KSPe(1, 0.5, 0.5),
      KSPmr(1, 0.5, 0.5),
      KSP(1, 0.5, 0.5),
      CSPmr(1, 0.5, 0.5),
      CSP(1, 0.5, 0.5)
    )

    val ndim = 2

    val references = Vector(
      // the categorical data sets
      IndependentCat(ndim, 0, "gaussian", 1),
      IndependentCat(ndim, 0, "gaussian", 2),
      IndependentCat(ndim, 0, "gaussian", 3),
      IndependentCat(ndim, 0, "gaussian", 5),
      IndependentCat(ndim, 0, "gaussian", 10),
      IndependentCat(ndim, 0, "gaussian", 20),
      // the ordinal data sets
      Independent(ndim, 0, "gaussian", 1),
      Independent(ndim, 0, "gaussian", 2),
      Independent(ndim, 0, "gaussian", 3),
      Independent(ndim, 0, "gaussian", 5),
      Independent(ndim, 0, "gaussian", 10),
      Independent(ndim, 0, "gaussian", 20),
      // the numeric data set
      Independent(ndim, 0, "gaussian", 0)
    )

    val generators = Vector(
      // the categorical data sets
      LinearCat(ndim, _, "gaussian", 1),
      LinearCat(ndim, _, "gaussian", 2),
      LinearCat(ndim, _, "gaussian", 3),
      LinearCat(ndim, _, "gaussian", 5),
      LinearCat(ndim, _, "gaussian", 10),
      LinearCat(ndim, _, "gaussian", 20),
      // the ordinal data sets
      Linear(ndim, _, "gaussian", 1),
      Linear(ndim, _, "gaussian", 2),
      Linear(ndim, _, "gaussian", 3),
      Linear(ndim, _, "gaussian", 5),
      Linear(ndim, _, "gaussian", 10),
      Linear(ndim, _, "gaussian", 20),
      // the numeric data set
      Linear(ndim, _, "gaussian", 0)
    )

    info(s"Dealing with {${references.map(_.id) mkString ","}}")
    for {
      rep <- 1 to nrep
    } {
      if(rep % 1000 == 0) {
        info(s"Reached rep = $rep")
      }
      compareContrast(references, tests, rep)
    }

    val gens = (0 to 10).toVector.flatMap(x => generators.map(y => y(x.toDouble/10.0)))
    info(s"Dealing with {${gens.map(_.id) mkString ","}}")
    for {
      rep <- 1 to nrep
    } {
      if(rep % 1000 == 0) {
        info(s"Reached rep = $rep")
      }
      compareContrast(gens, tests, rep)
    }

    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

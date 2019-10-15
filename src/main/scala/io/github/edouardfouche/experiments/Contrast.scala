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
package io.github.edouardfouche.experiments

import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object Contrast extends Experiment {
  val nrep = 10000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")


    val tests = Vector(
      MWP(1,0.5, 0.5),
      MWPn(1, 0.5, 0.5),
      MWPu(1, 0.5, 0.5),
      //MWPr(1,0.5, 0.5),
      //KSPs(1,0.5, 0.5),
      //KSPsn(1,0.5, 0.5),
      KSP(1, 0.5, 0.5),
      KSPn(1, 0.5, 0.5),
      KSPs(1, 0.5, 0.5),
      KSPsn(1, 0.5, 0.5),
      CSP(1, 0.5, 0.5),
      CSPn(1, 0.5, 0.5)
    )


    //val tests = Vector(
    //  MWPu(1, 0.5, 0.5)
    //)

    val ndim = 2 // So it seems I've got the same result with 3-d (verified in 2019-10-03-14-56_Contrast_)

    val references = Vector(
      // the categorical stuff
      IndependentCat(ndim, 0, "gaussian", 1),
      IndependentCat(ndim, 0, "gaussian", 2),
      IndependentCat(ndim, 0, "gaussian", 3),
      IndependentCat(ndim, 0, "gaussian", 5),
      IndependentCat(ndim, 0, "gaussian", 10),
      IndependentCat(ndim, 0, "gaussian", 20),
      // the ordinal stuff
      Independent(ndim, 0, "gaussian", 1),
      Independent(ndim, 0, "gaussian", 2),
      Independent(ndim, 0, "gaussian", 3),
      Independent(ndim, 0, "gaussian", 5),
      Independent(ndim, 0, "gaussian", 10),
      Independent(ndim, 0, "gaussian", 20),
      // the numeric stuff
      Independent(ndim, 0, "gaussian", 0)
    )

    val generators = Vector(
      // the categorical stuff
      LinearCat(ndim, _, "gaussian", 1),
      LinearCat(ndim, _, "gaussian", 2),
      LinearCat(ndim, _, "gaussian", 3),
      LinearCat(ndim, _, "gaussian", 5),
      LinearCat(ndim, _, "gaussian", 10),
      LinearCat(ndim, _, "gaussian", 20),
      // the ordinal stuff
      Linear(ndim, _, "gaussian", 1),
      Linear(ndim, _, "gaussian", 2),
      Linear(ndim, _, "gaussian", 3),
      Linear(ndim, _, "gaussian", 5),
      Linear(ndim, _, "gaussian", 10),
      Linear(ndim, _, "gaussian", 20),
      // the numeric stuff
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

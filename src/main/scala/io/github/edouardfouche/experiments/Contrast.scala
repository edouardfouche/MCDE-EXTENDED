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
      MWPr(1,0.5, 0.5),
      KSP(1,0.5, 0.5),
      KSPn(1,0.5, 0.5),
      KSPP(1,0.5, 0.5),
      CSP(1,0.5, 0.5)
    )

    val references = Vector(
      // the categorical stuff
      IndependentCat(2, 0, "gaussian", 5),
      IndependentCat(2, 0, "gaussian", 10),
      IndependentCat(2, 0, "gaussian", 20),
      // the ordinal stuff
      Independent(2, 0, "gaussian", 5),
      Independent(2, 0, "gaussian", 10),
      Independent(2, 0, "gaussian", 20),
      // the numeric stuff
      Independent(2, 0, "gaussian", 0)
    )

    val generators = Vector(
      // the categorical stuff
      LinearCat(2, _, "gaussian", 5),
      LinearCat(2, _, "gaussian", 10),
      LinearCat(2, _, "gaussian", 20),
      // the ordinal stuff
      Linear(2, _, "gaussian", 5),
      Linear(2, _, "gaussian", 10),
      Linear(2, _, "gaussian", 20),
      // the numeric stuff
      Linear(2, _, "gaussian", 0)
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

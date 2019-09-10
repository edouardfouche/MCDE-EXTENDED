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

import io.github.edouardfouche.experiments.Contrast.compareContrast
import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataRef

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object ContrastBeta extends Experiment {
  val beta_range = Vector(0.1, 0.2, 0.3, 0.5, 1)
  val nrep = 1000

  def run(): Unit = {

    for {
      beta <- beta_range
    } yield {
      info(s"Starting com.edouardfouche.experiments with configuration: alpha=${0.5}, beta=${beta}")

      val tests = Vector(
        MWP(1,0.5, beta),
        KSP(1,0.5, beta),
        CSP(1,0.5, beta)
      )

      val generators = Vector(
        Linear(2,0,"gaussian",10),
        Independent(2,0,"gaussian",10)
      )

      for {
        rep <- 1 to nrep
      } {
        compareContrast(generators, tests, rep)
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

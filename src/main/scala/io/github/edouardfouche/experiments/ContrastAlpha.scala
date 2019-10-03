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

import io.github.edouardfouche.generators.{Linear, _}
import io.github.edouardfouche.mcde._

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object ContrastAlpha extends Experiment {
  val alpha_range = Vector(0.1, 0.3, 0.5, 0.7, 0.9)
  val nrep = 1000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    for {
      alpha <- alpha_range
    } yield {
      info(s"Starting com.edouardfouche.experiments with configuration: alpha=${alpha}, beta=${0.5}")

      val tests = Vector(
        MWP(1,alpha, 0.5),
        KSP_bis(1, alpha, 0.5),
        CSP(1,alpha, 0.5)
      )

      val generators = Vector(
        Linear(3,0.1,"gaussian",0),
        Independent(3,0,"gaussian",0)
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

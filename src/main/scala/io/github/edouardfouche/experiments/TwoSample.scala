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
import io.github.edouardfouche.index.deprecated.generators.IndependentCat
import io.github.edouardfouche.index.dimension.{D_CRank, D_CRank_Stream, D_Count, D_Count_Stream, D_Rank, D_Rank_Stream}
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object TwoSample extends Experiment {
  val nrep = 100000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    val tests_Dindex = Vector(
      MWP(1,0.5, 0.5),
      KSP(1,0.5, 0.5),
      KSPP(1,0.5, 0.5),
      CSP(1,0.5, 0.5)
    )

    val ndim = 1
    val references = Vector(
      // the categorical stuff
      IndependentCat(ndim, 0, "gaussian", 2),
      IndependentCat(ndim, 0, "gaussian", 3),
      IndependentCat(ndim, 0, "gaussian", 5),
      IndependentCat(ndim, 0, "gaussian", 10),
      IndependentCat(ndim, 0, "gaussian", 20),
      // the ordinal stuff
      Independent(ndim, 0, "gaussian", 2),
      Independent(ndim, 0, "gaussian", 3),
      Independent(ndim, 0, "gaussian", 5),
      Independent(ndim, 0, "gaussian", 10),
      Independent(ndim, 0, "gaussian", 20),
      // the numeric stuff
      Independent(ndim, 0, "gaussian", 0)
    )

    for {
      ref <- references.par
    } {
      for {
        rep <- 1 to nrep
      } {
        if(rep % 10000 == 0) {
          info(s"${ref.id} -> Reached rep $rep")
        }
        val raw = ref.generate(1000).transpose.head
        // Save data samples (debugging purpose)
        //utils.createFolderIfNotExisting(experiment_folder + "/data")
        //if (rep == 1) utils.saveDataSet(raw.columns.transpose, experiment_folder + "/data/" + s"${generator.id}")

        for {
          test <- tests_Dindex
        } {
          val (prepcpu, prepwall, dindex) = StopWatch.measureTime(test.getDIndexConstruct(raw))
          val randomboolean: Array[Boolean] = (1 to dindex.length).map(x => math.random < 0.50).toArray
          val (runcpu, runwall, contrast) = StopWatch.measureTime(test.twoSample(dindex, randomboolean))

          val attributes = List("refId", "testId", "prepcpu", "prepwall",
            "runcpu", "runwall", "contrast", "rep")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", ref.id)
          summary.add("testId", test.id)
          summary.add("prepcpu", prepcpu)
          summary.add("prepwall", prepwall)
          summary.add("runcpu", runcpu)
          summary.add("runwall", runwall)
          summary.add("contrast", contrast)
          summary.add("rep", rep)
          summary.write(summaryPath)
        }
      }
    }

    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

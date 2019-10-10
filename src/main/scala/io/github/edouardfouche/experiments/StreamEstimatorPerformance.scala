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
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object StreamEstimatorPerformance extends Experiment {
  val nrep = 1
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    val tests = Vector(
      MWP(1, 0.5, 0.5),
      MWP(5, 0.5, 0.5),
      MWP(10, 0.5, 0.5),
      MWP(50, 0.5, 0.5),
      MWP(100, 0.5, 0.5),
      MWP(200, 0.5, 0.5)
      //MWPn(1, 0.5, 0.5),
      //MWPr(1,0.5, 0.5),
      //KSPs(1,0.5, 0.5),
      //KSPsn(1,0.5, 0.5),
      //KSP(1, 0.5, 0.5),
      //KSPn(1, 0.5, 0.5),
      //KSPs(1, 0.5, 0.5),
      //KSPsn(1, 0.5, 0.5),
      //CSP(1, 0.5, 0.5),
      //CSPn(1, 0.5, 0.5)
    )

    val streamestimators: Vector[McdeStats => StreamEstimator] = Vector(
      StreamEstimator(_, 1000, 500, 0.8, true),
      StreamEstimator(_, 1000, 100, 0.8, true),
      StreamEstimator(_, 1000, 50, 0.8, true),
      StreamEstimator(_, 1000, 10, 0.8, true),
      StreamEstimator(_, 1000, 500, 0.9, true),
      StreamEstimator(_, 1000, 100, 0.9, true),
      StreamEstimator(_, 1000, 50, 0.9, true),
      StreamEstimator(_, 1000, 10, 0.9, true),
      StreamEstimator(_, 1000, 500, 0.99, true),
      StreamEstimator(_, 1000, 100, 0.99, true),
      StreamEstimator(_, 1000, 50, 0.99, true),
      StreamEstimator(_, 1000, 10, 0.99, true),
      StreamEstimator(_, 1000, 500, 1, true),
      StreamEstimator(_, 1000, 100, 1, true),
      StreamEstimator(_, 1000, 50, 1, true),
      StreamEstimator(_, 1000, 10, 1, true),

      StreamEstimator(_, 1000, 500, 0.8, false),
      StreamEstimator(_, 1000, 100, 0.8, false),
      StreamEstimator(_, 1000, 50, 0.8, false),
      StreamEstimator(_, 1000, 10, 0.8, false),
      StreamEstimator(_, 1000, 500, 0.9, false),
      StreamEstimator(_, 1000, 100, 0.9, false),
      StreamEstimator(_, 1000, 50, 0.9, false),
      StreamEstimator(_, 1000, 10, 0.9, false),
      StreamEstimator(_, 1000, 500, 0.99, false),
      StreamEstimator(_, 1000, 100, 0.99, false),
      StreamEstimator(_, 1000, 50, 0.99, false),
      StreamEstimator(_, 1000, 10, 0.99, false),
      StreamEstimator(_, 1000, 500, 1, false),
      StreamEstimator(_, 1000, 100, 1, false),
      StreamEstimator(_, 1000, 50, 1, false),
      StreamEstimator(_, 1000, 10, 1, false)
    )

    val ndim = 2 // So it seems I've got the same result with 3-d (verified in 2019-10-03-14-56_Contrast_)

    def generateSmoothSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray

    def generateSmoothSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray

    def generateAbruptSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray

    def generateAbruptSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray

    val slowchanging: Array[Array[Double]] = (generateSmoothSlopUp ++ generateSmoothSlopDown ++
      generateSmoothSlopUp ++ generateSmoothSlopDown ++
      generateSmoothSlopUp ++ generateSmoothSlopDown ++
      generateSmoothSlopUp ++ generateSmoothSlopDown ++
      generateSmoothSlopUp ++ generateSmoothSlopDown).transpose

    val fastchanging: Array[Array[Double]] = (generateAbruptSlopUp ++ generateAbruptSlopDown ++
      generateAbruptSlopUp ++ generateAbruptSlopDown ++
      generateAbruptSlopUp ++ generateAbruptSlopDown ++
      generateAbruptSlopUp ++ generateAbruptSlopDown ++
      generateAbruptSlopUp ++ generateAbruptSlopDown).transpose

    for {
      test <- tests
    } {
      for {
        streamestimator <- streamestimators
      } {
        val estimator = streamestimator(test)
        val (slowcpu, slowwall, slowoutput: Array[Double]) = StopWatch.measureTime(estimator.run(new DataSet(slowchanging)))
        val (fastcpu, fastwall, fastoutput: Array[Double]) = StopWatch.measureTime(estimator.run(new DataSet(fastchanging)))

        utils.createFolderIfNotExisting(experiment_folder + "/data")
        val slowpath = "data/" + s"slow-${estimator.id}"
        val fastpath = "data/" + s"fast-${estimator.id}"
        utils.saveDataSet(Array(slowoutput), experiment_folder + "/" + slowpath)
        utils.saveDataSet(Array(fastoutput), experiment_folder + "/" + fastpath)

        val attributes = List("estimatorId", "slowcpu", "slowwall", "fastcpu", "fastwall", "slowpath", "fastpath")
        val summary = ExperimentSummary(attributes)
        summary.add("estimatorId", estimator.id)
        summary.add("slowcpu", slowcpu)
        summary.add("slowwall", slowwall)
        summary.add("fastcpu", fastcpu)
        summary.add("fastwall", fastwall)
        summary.add("slowpath", slowpath)
        summary.add("fastpath", fastpath)

        summary.write(summaryPath)
      }

    }


    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

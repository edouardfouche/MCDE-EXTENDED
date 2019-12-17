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
import io.github.edouardfouche.mcde.{StreamEstimator, _}
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch

/**
  * Created by fouchee on 12.07.17.
  * Test the performance of monitoring on a data stream
  */
object StreamEstimatorPerformance extends Experiment {
  val nrep = 50

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    val tests = Vector(
      MWP(1, 0.5, 0.5),
      MWP(5, 0.5, 0.5),
      MWP(10, 0.5, 0.5),
      MWP(50, 0.5, 0.5),
      MWP(100, 0.5, 0.5),
      MWP(500, 0.5, 0.5)
    )

    val estimators: Vector[(McdeStats, Boolean) => StreamEstimator] = Vector(
      StreamEstimator(_, 1000, 1000, 0.99, _),
      StreamEstimator(_, 1000, 500, 0.99, _),
      StreamEstimator(_, 1000, 100, 0.99, _),
      StreamEstimator(_, 1000, 50, 0.99, _),
      StreamEstimator(_, 1000, 10, 0.99, _),
      StreamEstimator(_, 1000, 1, 0.99, _)
    )

    val ndim = 3

    def runestimator(estimator: Boolean => StreamEstimator, data: Array[Array[Double]], rep: Int, ref: (Double, Array[Double]) = (0, Array())): (Double, Array[Double]) = {
      val streamestimator = estimator(true)
      val staticestimator = estimator(false)
      info(s"Starting with ${streamestimator.id}")
      val (staticcpu, staticwall, staticoutput: Array[Double]) = StopWatch.measureTime(staticestimator.run(new DataSet(data)))
      val (streamcpu, streamwall, streamoutput: Array[Double]) = StopWatch.measureTime(streamestimator.run(new DataSet(data)))

      val streampath = "data/" + s"${streamestimator.id}"
      val staticpath = "data/" + s"${staticestimator.id}"
      if (rep == 1) {
        utils.createFolderIfNotExisting(experiment_folder + "/data")
        utils.save(streamoutput.map(x => (math rint x * 1000) / 1000), experiment_folder + "/" + streampath)
        utils.save(staticoutput.map(x => (math rint x * 1000) / 1000), experiment_folder + "/" + staticpath)
      }

      val relerror: Double = streamoutput.zip(staticoutput).map(x => math.abs(x._1 - x._2)).sum / streamoutput.length
      val relmeansqerror: Double = streamoutput.zip(staticoutput).map(x => math.pow(x._1 - x._2, 2)).sum / streamoutput.length
      val relspeedup: Double = staticcpu / streamcpu

      val streamabserror: Double = if (ref._1 == 0) 0 else streamoutput.zip(ref._2).map(x => math.abs(x._1 - x._2)).sum / streamoutput.length
      val streamabsmeansqerror: Double = if (ref._1 == 0) 0 else streamoutput.zip(ref._2).map(x => math.pow(x._1 - x._2, 2)).sum / streamoutput.length
      val streamabsspeedup: Double = ref._1 / streamcpu

      val staticabserror: Double = if (ref._1 == 0) 0 else staticoutput.zip(ref._2).map(x => math.abs(x._1 - x._2)).sum / staticoutput.length
      val staticabsmeansqerror: Double = if (ref._1 == 0) 0 else staticoutput.zip(ref._2).map(x => math.pow(x._1 - x._2, 2)).sum / staticoutput.length
      val staticabsspeedup: Double = ref._1 / staticcpu

      val attributes = List("estimatorId", "cpu", "wall", "abserror", "absmeansqerror", "absspeedup", "relerror", "relmeansqerror", "relspeedup", "path", "rep")
      val summary = ExperimentSummary(attributes)
      summary.add("estimatorId", streamestimator.id)
      summary.add("cpu", "%.6f".format(streamcpu))
      summary.add("wall", "%.6f".format(streamwall))
      summary.add("abserror", "%.6f".format(streamabserror))
      summary.add("absmeansqerror", "%.6f".format(streamabsmeansqerror))
      summary.add("absspeedup", "%.6f".format(streamabsspeedup))
      summary.add("relerror", "%.6f".format(relerror))
      summary.add("relmeansqerror", "%.6f".format(relmeansqerror))
      summary.add("relspeedup", "%.6f".format(relspeedup))
      summary.add("path", streampath)
      summary.add("rep", rep)

      summary.write(summaryPath)

      val summary2 = ExperimentSummary(attributes)
      summary2.add("estimatorId", staticestimator.id)
      summary2.add("cpu", "%.6f".format(staticcpu))
      summary2.add("wall", "%.6f".format(staticwall))
      summary2.add("abserror", "%.6f".format(staticabserror))
      summary2.add("absmeansqerror", "%.6f".format(staticabsmeansqerror))
      summary2.add("absspeedup", "%.6f".format(staticabsspeedup))
      summary2.add("relmeansqerror", 0)
      summary2.add("relerror", 0)
      summary2.add("relspeedup", 1)
      summary2.add("path", staticpath)
      summary2.add("rep", rep)

      summary2.write(summaryPath)

      (staticcpu, staticoutput)
    }

    for {
      rep <- (1 to nrep).par
    } {
      info(s"Starting rep: $rep")
      val slowchanging: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray.transpose

      val reftest = tests.last
      val refestimator = estimators.last

      info(s"Handling reference (rep: ${-rep})")
      val reference = runestimator(refestimator(reftest, _), slowchanging, -rep)

      for {
        test <- tests.par
      } {
        for {
          estimator <- (estimators).par
        } {
          runestimator(estimator(test, _), slowchanging, rep, reference)
        }
      }
      info(s"Done with rep $rep")
    }

    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

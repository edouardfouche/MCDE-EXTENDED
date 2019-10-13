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
import org.slf4j.MDC

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object Power extends Experiment {
  val nrep = 1000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments ${this.getClass.getSimpleName}")

    /*
    val tests = Vector(
      MWP(1, 0.5, 0.5),
      MWPn(1, 0.5, 0.5),
      MWPu(1, 0.5, 0.5),
      //MWPr(1,0.5, 0.5),
      KSPs(1, 0.5, 0.5),
      KSPsn(1, 0.5, 0.5),
      KSP(1, 0.5, 0.5),
      KSPn(1, 0.5, 0.5),
      CSP(1, 0.5, 0.5),
      CSPn(1, 0.5, 0.5)
    )
     */

    val tests = Vector(
      MWPu(1, 0.5, 0.5),
    )

    val ndims = Array(2, 3, 5, 10, 20)

    val constructors: Vector[(Int, Double) => DataGenerator] = Vector(
      // the categorical stuff
      LinearCat(_, _, "gaussian", 1),
      LinearCat(_, _, "gaussian", 2),
      LinearCat(_, _, "gaussian", 3),
      LinearCat(_, _, "gaussian", 5),
      LinearCat(_, _, "gaussian", 10),
      LinearCat(_, _, "gaussian", 20),
      // the ordinal stuff
      Linear(_, _, "gaussian", 1),
      Linear(_, _, "gaussian", 2),
      Linear(_, _, "gaussian", 3),
      Linear(_, _, "gaussian", 5),
      Linear(_, _, "gaussian", 10),
      Linear(_, _, "gaussian", 20),
      // the numeric stuff
      Linear(_, _, "gaussian", 0)
    )

    for {
      constructor <- constructors
    } {
      for {
        ndim <- ndims
        level <- 0 to 30
      } {
        val generators = constructors.map(y => y(ndim, "%.2f".format(level.toDouble / 30.0).toDouble))
        info(s"Generators: {${generators.map(_.id) mkString ","}}")
        for {
          generator <- generators.par
        } {
          MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")
          val precpus: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
          val runcpus: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
          val contrasts: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
          for {
            rep <- 1 to nrep
          } {
            val raw = new DataSet(generator.generate(1000).transpose, types = (1 to generator.nDim).toArray.map(x => "c"))
            // Save data samples (debugging purpose)
            utils.createFolderIfNotExisting(experiment_folder + "/data")
            if (rep == 1) utils.saveDataSet(raw.columns.transpose, experiment_folder + "/data/" + s"${generator.id}")
            for {
              test <- tests
            } {
              val (prepCPUtime, prepWalltime, data) = StopWatch.measureTime(test.preprocess(raw))
              val (runCPUtime, runWalltime, contrast) = StopWatch.measureTime(test.contrast(data, (0 until generator.nDim).toSet))
              precpus(test.id) = prepCPUtime :: precpus(test.id)
              runcpus(test.id) = runCPUtime :: runcpus(test.id)
              contrasts(test.id) = contrast :: contrasts(test.id)
            }
          }

          for {
            test <- tests
          } {
            val avgprecpu = precpus(test.id).sum / nrep
            val stdpre = breeze.stats.stddev(precpus(test.id))
            val avgruncpu = runcpus(test.id).sum / nrep
            val stdrun = breeze.stats.stddev(runcpus(test.id))
            val avgcon = contrasts(test.id).sum / nrep
            val stdcon = breeze.stats.stddev(contrasts(test.id))
            val power = contrasts(test.id).count(_ > 0.95).toDouble / nrep

            val attributes = List("refId", "testId", "ncols", "avgprecpu", "stdpre", "avgruncpu", "stdrun", "avgcon", "stdcon", "power")
            val summary = ExperimentSummary(attributes)
            summary.add("refId", generator.id)
            summary.add("testId", test.id)
            summary.add("ncols", generator.nDim)
            summary.add("avgprecpu", "%.6f".format(avgprecpu))
            summary.add("stdpre", "%.6f".format(stdpre))
            summary.add("avgruncpu", "%.6f".format(avgruncpu))
            summary.add("stdrun", "%.6f".format(stdrun))
            summary.add("avgcon", "%.6f".format(avgcon))
            summary.add("stdcon", "%.6f".format(stdcon))
            summary.add("power", "%.6f".format(power))
            summary.write(summaryPath)
          }
          info(s"Done with ${generator.id}")
        }
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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

import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch
//import com.edouardfouche.generators_deprecated.{DataGenerator, GeneratorFactory, Independent}
import io.github.edouardfouche.generators.DataGenerator


/**
  * Created by fouchee on 12.07.17.
  * Check the power of every approach against a selected number of generators_deprecated
  */
object Power extends Experiment {
  val nrep = 1000 // number of data sets we use to estimate rejection rate
  val ndims = Array(2, 3, 5, 10, 20)
  val noiseLevels = 30
  val generators: Vector[(Int, Double, String, Int) => DataGenerator] = selected_generators
  val m = 50
  val n = 1000

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting com.edouardfouche.experiments - ${this.getClass.getSimpleName}")
    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val tests = Vector(
      MWP(1, 0.5, 0.5),
      MWPn(1, 0.5, 0.5),
      MWPu(1, 0.5, 0.5),
      //MWPr(1,0.5, 0.5),
      KSP(1, 0.5, 0.5),
      KSPn(1, 0.5, 0.5),
      KSPs(1, 0.5, 0.5),
      KSPsn(1, 0.5, 0.5),
      CSP(1, 0.5, 0.5),
      CSPn(1, 0.5, 0.5)
    )

    for {
      ndim <- ndims
      level <- 0 to 30
    } {
      info(s"Starting with ndim:$ndim and level:$level")
      for {
        generator <- generators.par
      } {
        val precpus: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
        val runcpus: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
        val contrasts: scala.collection.mutable.Map[String, List[Double]] = scala.collection.mutable.Map(tests.map(x => (x.id, List[Double]())): _*)
        //val gen = generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 0)

        for {
          test <- tests.par
        } {
          for {
            rep <- 1 to nrep
          } {
            val gen = if (test.id contains "CSP") generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 10)
            else generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 0)
            val generated_data: Array[Array[Double]] = gen.generate(1000).transpose
            val raw = new DataSet(generated_data.map(x => x.map(y => if (y.isNaN) 0.0 else y))) //TODO: Quick fix in case of NaNs (resulting from overflows or badly constructed generator I guess)
            // Save data samples (debugging purpose)
            //utils.createFolderIfNotExisting(experiment_folder + "/data")
            //if (rep == 1) utils.saveDataSet(raw.columns.transpose, experiment_folder + "/data/" + s"${gen.id}")
            val (prepCPUtime, prepWalltime, data) = StopWatch.measureTime(test.preprocess(raw))
            val (runCPUtime, runWalltime, contrast) = StopWatch.measureTime(test.contrast(data, (0 until gen.nDim).toSet))
            precpus(test.id) = prepCPUtime :: precpus(test.id)
            runcpus(test.id) = runCPUtime :: runcpus(test.id)
            contrasts(test.id) = contrast :: contrasts(test.id)
          }
        }

        for {
          test <- tests
        } {
          val gen = if (test.id contains "CSP") generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 10)
          else generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 0)

          val avgprecpu = precpus(test.id).sum / nrep
          val stdpre = breeze.stats.stddev(precpus(test.id))
          val avgruncpu = runcpus(test.id).sum / nrep
          val stdrun = breeze.stats.stddev(runcpus(test.id))
          val avgcon = contrasts(test.id).sum / nrep
          val stdcon = breeze.stats.stddev(contrasts(test.id))
          val power = contrasts(test.id).count(_ > 0.95).toDouble / nrep

          val attributes = List("refId", "testId", "ncols", "avgprecpu", "stdpre", "avgruncpu", "stdrun", "avgcon", "stdcon", "power")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", gen.id)
          summary.add("testId", test.id)
          summary.add("ncols", gen.nDim)
          summary.add("avgprecpu", "%.6f".format(avgprecpu))
          summary.add("stdpre", "%.6f".format(stdpre))
          summary.add("avgruncpu", "%.6f".format(avgruncpu))
          summary.add("stdrun", "%.6f".format(stdrun))
          summary.add("avgcon", "%.6f".format(avgcon))
          summary.add("stdcon", "%.6f".format(stdcon))
          summary.add("power", "%.6f".format(power))
          summary.write(summaryPath)
        }
        info(s"Done with ${generator(ndim, "%.2f".format(level.toDouble / 30.0).toDouble, "gaussian", 0).id}(/10)")
      }
      info(s"Done with ndim:$ndim and level:$level")
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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

import java.io.FileWriter

import io.github.edouardfouche.experiments.Data._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object StreamMiner extends Experiment {
  val nrep = 1
  val windowsize = 1000
  val stepsize = 50
  val gamma = 0.99
  val nthread = 0 // try it out
  val test = MWP(50, 0.5, 0.5, nthread)
  val bioliqdata = bioliq // replace with your own data set, see also "experiments/Data".

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    info("Opening data set...")
    val dataset: DataSet = bioliqdata.open()
    info(s"nrows: ${dataset.nrows}, ncols: ${dataset.ncols}")

    def runestimator(subspaces: Array[List[Int]]) = {
      val initdataset = dataset.take(windowsize)
      val restdataset = dataset.drop(windowsize)
      val index: test.I = test.preprocess(initdataset, stream = true)

      // first iteration
      for {sub <- subspaces} {
        val newcontrast = test.contrast(index, sub.toSet)
        val path = "data/" + s"${bioliqdata.id}-${sub mkString ";"}"
        // write it
        val fw = new FileWriter(experiment_folder + "/" + path, true);
        fw.write(s"${(math rint newcontrast * 1000) / 1000}");
        fw.close()
      }

      @tailrec
      def cumulative_contrast(data: DataSet,
                              acc: Int): Unit = {
        if (data.nrows != 0) {
          if (acc % 1000 == 0) println(s"${formatter.format(java.util.Calendar.getInstance().getTime)}, reached $acc")

          if (acc % stepsize == 0) {
            index.insert(data.head)

            for {sub <- subspaces} { // How to parallelize that?
              val newcontrast = test.contrast(index, sub.toSet)
              val path = "data/" + s"${bioliqdata.id}-${sub mkString ";"}"
              // write it
              val fw = new FileWriter(experiment_folder + "/" + path, true);
              fw.write(s",${(math rint newcontrast * 1000) / 1000}");
              fw.close()
            }
            cumulative_contrast(data.tail, acc + 1)
          } else {
            index.insert(data.head)
            // Do nothing
            cumulative_contrast(data.tail, acc + 1)
          }
        }
      }

      cumulative_contrast(restdataset, 0)

      for {sub <- subspaces} {
        val path = "data/" + s"${bioliq.id}-${sub mkString ";"}"
        val attributes = List("windowsize", "stepsize", "path", "subspace")
        val summary = ExperimentSummary(attributes)
        summary.add("windowsize", windowsize)
        summary.add("stepsize", stepsize)
        summary.add("path", path)
        summary.add("subspace", sub mkString ";")
        summary.write(summaryPath)
      }
    }

    val subspaces: Array[List[Int]] = for {
      x <- (0 until dataset.ncols).toArray
      y <- 0 until x
    } yield {
      List(x, y)
    }

    utils.createFolderIfNotExisting(experiment_folder + "/data")
    runestimator(subspaces)

    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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

import java.io.FileWriter

import io.github.edouardfouche.experiments.Data._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object StreamEstimatorMiner extends Experiment {
  val nrep = 1
  val windowsize = 1000
  val stepsize = 50
  val gamma = 0.99
  val nthread = 1
  val test = KSPsn(50, 0.5, 0.5, nthread)
  val bioliqdata = bioliq_interesting
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    /*
    val staticestimators: Vector[McdeStats => StreamEstimator] = Vector(
      StreamEstimator(_, 1000, 1000, 0, false),
      StreamEstimator(_, 1000, 500, 0, false),
      StreamEstimator(_, 1000, 100, 0, false),
      StreamEstimator(_, 1000, 50, 0, false),
      StreamEstimator(_, 1000, 10, 0, false),
      StreamEstimator(_, 1000, 1, 0, false)
    )
     */

    //val ndim = 3 //

    //def generateSmoothSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray
    //def generateSmoothSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray
    //def generateAbruptSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray
    //def generateAbruptSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray

    /*
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
      */

    //val slowchanging: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray.transpose
    //val fastchanging: Array[Array[Double]] = (Linear(ndim, 0, "gaussian", 0).generate(50000) ++
    //  Linear(ndim, 1, "gaussian", 0).generate(50000)).transpose

    info("Opening data set...")
    val dataset: DataSet = bioliqdata.open() // TODO: What is wrong with the full one?
    info(s"nrows: ${dataset.nrows}, ncols: ${dataset.ncols}")

    def runestimator(subspaces: Array[List[Int]]) = {
      //val estimator = streamestimator(test)
      //val subdataset = new DataSet(subspace.map(x => dataset(x)))
      //info(s"nrows: ${subdataset.nrows}, ncols: ${subdataset.ncols}")
      //val (slowcpu, slowwall, slowoutput: Array[Double]) = StopWatch.measureTime(estimator.run(subdataset))
      //val (slowcpu, slowwall, slowoutput: scala.collection.mutable.Map[List[Int],Array[Double]]) = StopWatch.measureTime(estimator.runMultiple(dataset, subspaces))

      val initdataset = dataset.take(windowsize)
      val restdataset = dataset.drop(windowsize)
      val index: test.I = test.preprocess(initdataset, stream = true)

      // First iteration
      /*
      for{subgroup <- subspaces.grouped(subspaces.length / nthread).toArray.par} {
        for{sub <- subgroup} { // parallelized
          val newcontrast =  test.contrast(index, sub.toSet)
          val path = "data/" + s"${bioliq_full.id}-${sub mkString ";"}"
          // write it
          val fw = new FileWriter(experiment_folder + "/" + path,  true) ;
          fw.write(s"${(math rint newcontrast * 1000) / 1000}") ;
          fw.close()
        }
      }
       */

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
            //println("insert with contrast computation")
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
            //println("insert without contrast computation")
            index.insert(data.head)
            // Do nothing
            cumulative_contrast(data.tail, acc + 1)
          }
        }
      }


      cumulative_contrast(restdataset, 0)

      for {sub <- subspaces} {
        val path = "data/" + s"${bioliq_full.id}-${sub mkString ";"}"

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

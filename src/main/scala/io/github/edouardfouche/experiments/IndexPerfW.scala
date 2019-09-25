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

import breeze.stats.distributions.{Gaussian, Uniform}
import io.github.edouardfouche.generators._
import io.github.edouardfouche.index.dimension._
import io.github.edouardfouche.utils.StopWatch

import scala.annotation.tailrec

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object IndexPerfW extends Experiment {
  val nrep = 1000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {

    info(s"Starting com.edouardfouche.experiments")

    val indexesconstructors = Vector(
      new D_Count(_),
      new D_Count_Stream(_),
      new D_CRank(_),
      new D_CRank_Stream(_),
      new D_Rank(_),
      new D_Rank_Stream(_)
    )

    //val windowsizes = Vector(100, 1000, 10000)

    @tailrec
    def gaussianprocess(start: Double, current: Array[Double], n: Int): Array[Double] = {
      if (n == 0) return current
      val newval: Double = start + Gaussian(0, 0.05).draw()

      val inboundval: Double = if ((newval <= 1) && (newval >= 0)) {
        newval
      } else if (newval < 0) {
        -newval
      } else {
        1 - (newval - 1)
      }
      gaussianprocess(inboundval, current :+ inboundval, n - 1)
    }

    val datasets: Vector[Array[Double]] = Vector(
      // the numeric stuff
      Independent(1, 0, "gaussian", 0).generate(20000).transpose.head,
      // the ordinal stuff
      //Independent(1,0,"gaussian",5),
      Independent(1, 0, "gaussian", 10).generate(20000).transpose.head,
      //Independent(1,0,"gaussian",20),
      // the gaussian process
      gaussianprocess(Uniform(0, 1).draw(), Array[Double](), 20000) //TODO: In fact I am not sure that this is changing anything w.r.t. the first case

      // the categorical stuff
      //IndependentCat(1,0,"gaussian",5),
      // IndependentCat(1,0,"gaussian",10), // Basically same results as with ordinal
      //IndependentCat(1,0,"gaussian",20),
    )

    for {
      windowsize <- (100 to 10000)
    } {

      for {
        (dataset, i) <- datasets.zipWithIndex
      } {
        info(s"Starting with windowsize: $windowsize, dataset: ${i}")
        // initialize the indexes
        val initdata: Array[Double] = dataset.take(windowsize)

        val measures = scala.collection.mutable.Map[String, Double]()
        val rmeasures = scala.collection.mutable.Map[String, Double]()

        val indexes: Vector[DimensionIndex] = indexesconstructors.map(x => {
          val (cpu, wall, index) = StopWatch.measureTime(x(initdata))
          val (rcpu, rwall, a) = StopWatch.measureTime(index.refresh())
          val attributes = List("refId", "indexId", "w", "cpu", "wall", "rcup", "rwall", "rep")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", i + "_init")
          summary.add("indexId", index.id)
          summary.add("w", windowsize)
          summary.add("cpu", "%.6f".format(cpu))
          summary.add("wall", "%.6f".format(wall))
          summary.add("rcpu", "%.6f".format(rcpu))
          summary.add("rwall", "%.6f".format(rwall))
          summary.add("rep", 0)
          summary.write(summaryPath)
          measures(index.id) = cpu
          rmeasures(index.id) = rcpu
          index
        })

        for {
          n <- (1 to nrep)
        } {
          val newpoint: Double = dataset(windowsize + n - 1)
          //if(newpoint.isNaN) println("generator produced a NaN !")
          for {
            index <- indexes.par
          } {
            val (cpu, wall, a) = StopWatch.measureTime(index.insert(newpoint))
            val (rcpu, rwall, b) = StopWatch.measureTime(index.refresh())
            //val (rcpu, rwall, b) = StopWatch.measureTime({})
            val attributes = List("refId", "indexId", "w", "cpu", "wall", "rcup", "rwall", "rep")
            val summary = ExperimentSummary(attributes)
            summary.add("refId", i)
            summary.add("indexId", index.id)
            summary.add("w", windowsize)
            summary.add("cpu", "%.6f".format(cpu))
            summary.add("wall", "%.6f".format(wall))
            summary.add("rcpu", "%.6f".format(rcpu))
            summary.add("rwall", "%.6f".format(rwall))
            summary.add("rep", n)
            summary.write(summaryPath)
            measures(index.id) = measures(index.id) + cpu
            rmeasures(index.id) = rmeasures(index.id) + rcpu
          }
        }
        info(s"Avg ins cpu (ms): ${measures.mapValues(x => "%.6f".format(x / nrep)).mkString(",")}")
        info(s"Avg ref cpu (ms): ${rmeasures.mapValues(x => "%.6f".format(x / nrep)).mkString(",")}")
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

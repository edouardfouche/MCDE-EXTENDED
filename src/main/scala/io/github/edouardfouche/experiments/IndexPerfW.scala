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

import breeze.stats.distributions.Gaussian
import io.github.edouardfouche.generators._
import io.github.edouardfouche.index.dimension._
import io.github.edouardfouche.utils.StopWatch

import scala.annotation.tailrec

/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object IndexPerfW extends Experiment {
  val nrep = 100
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

    val generators: Vector[DataGenerator] = Vector( // the numeric stuff
      Independent(1, 0, "gaussian", 0),
      // the ordinal stuff
      //Independent(1,0,"gaussian",5),
      Independent(1, 0, "gaussian", 20),
      //Independent(1,0,"gaussian",20),
      // the gaussian process
      //gaussianprocess(Uniform(0, 1).draw(), Array[Double](), 200000) //TODO: In fact I am not sure that this is changing anything w.r.t. the first case

      // the categorical stuff
      //IndependentCat(1,0,"gaussian",5),
      IndependentCat(1, 0, "gaussian", 10) // Basically same results as with ordinal
      //IndependentCat(1,0,"gaussian",20),
    )

    val datasets: Vector[(Array[Double], String)] = generators.map(x => (x.generate(20000).transpose.head, x.id))

    for {
      windowsize <- (100 to 10000)
    } {

      for {
        (dataset, id) <- datasets
      } {
        info(s"Starting with windowsize: $windowsize, dataset: ${id}, nrep: $nrep")
        // initialize the indexes
        val initdata: Array[Double] = dataset.take(windowsize)

        val measures = scala.collection.mutable.Map[String, Array[Double]]()
        val rmeasures = scala.collection.mutable.Map[String, Array[Double]]()

        val indexes: Vector[DimensionIndex] = indexesconstructors.map(x => {
          val (cpu, wall, index) = StopWatch.measureTime(x(initdata))
          val (rcpu, rwall, a) = StopWatch.measureTime(index.refresh())
          val attributes = List("refId", "indexId", "w", "avg_cpu", "avg_rcup", "std_cpu", "std_rcup")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", id + "-init")
          summary.add("indexId", index.id)
          summary.add("w", windowsize)
          summary.add("avg_cpu", "%.6f".format(cpu))
          summary.add("avg_rcup", "%.6f".format(rcpu))
          summary.add("std_cpu", 0)
          summary.add("std_rcup", 0)
          //summary.add("rep", 0)
          summary.write(summaryPath)
          measures(index.id) = Array[Double]()
          rmeasures(index.id) = Array[Double]()
          index
        })

        for {
          n <- (1 to nrep)
        } {
          val newpoint: Double = dataset(windowsize + n - 1)
          //if(newpoint.isNaN) println("generator produced a NaN !")
          for {
            index <- indexes
          } {
            val (cpu, wall, a) = StopWatch.measureTime(index.insert(newpoint))
            val (rcpu, rwall, b) = StopWatch.measureTime(index.refresh())
            //val (rcpu, rwall, b) = StopWatch.measureTime({})
            measures(index.id) = measures(index.id) :+ cpu
            rmeasures(index.id) = rmeasures(index.id) :+ rcpu
          }
        }
        for {
          index <- measures.keys
        } {
          val attributes = List("refId", "indexId", "w", "avg_cpu", "avg_rcup", "std_cpu", "std_rcup")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", id)
          summary.add("indexId", index)
          summary.add("w", windowsize)
          summary.add("avg_cpu", "%.6f".format(measures(index).sum / measures(index).length))
          //summary.add("avg_wall", "%.6f".format(wall))
          summary.add("avg_rcup", "%.6f".format(rmeasures(index).sum / rmeasures(index).length))
          summary.add("std_cpu", "%.6f".format(breeze.stats.stddev(measures(index))))
          summary.add("std_rcup", "%.6f".format(breeze.stats.stddev(rmeasures(index))))
          //summary.add("rwall", "%.6f".format(rwall))
          //summary.add("rep", n)
          summary.write(summaryPath)
        }

        info(s"Avg ins cpu (ms): ${measures.toArray.sortBy(_._1).map(x => s"${x._1} -> " + "%.6f".format(x._2.sum / nrep)).mkString(", ")} ($id)")
        info(s"Avg ref cpu (ms): ${rmeasures.toArray.sortBy(_._1).map(x => s"${x._1} -> " + "%.6f".format(x._2.sum / nrep)).mkString(", ")} ($id)")
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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
import io.github.edouardfouche.index.dimension._
import io.github.edouardfouche.utils.StopWatch


/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object PerformanceIndex extends Experiment {
  val nrep = 1000
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {
    info(s"Starting com.edouardfouche.experiments ${this.getClass.getSimpleName}")

    val indexes: Vector[Array[Double] => DimensionIndex] = Vector(
      new D_Count(_),
      new D_Count_Stream(_),
      new D_CRank(_),
      new D_CRank_Stream(_),
      new D_Rank(_),
      new D_Rank_Stream(_)
    )
    info(s"initialize indexes")
    val generators: Vector[DataGenerator] = Vector (
      Independent(1, 0, "gaussian", 10),
      Independent(1, 0, "gaussian", 10),
      Independent(1, 0, "gaussian", 20),
      Independent(1, 0, "gaussian", 20),
      Independent(1, 0, "gaussian", 0),
      Independent(1, 0, "gaussian", 0)
    )
    info(s"initialize generators")

    //val datasets: Vector[(Array[Double], String)] = generators.map(x => (x.generate(200000).transpose.head, x.id))

    for {
      i <- indexes.indices.par
    } {
      val index = indexes(i)
      val generator = generators(i)
      //MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")
      info(s"Starting with index: ${index(Array(1,2,3)).id}")

      for {
        windowsize <- (100 to 100000) by 100
      } {
        val dataset = generator.generate(windowsize + nrep).transpose.head
        val initdata: Array[Double] = dataset.take(windowsize)

        val (cpu, wall, initalizedindex) = StopWatch.measureTime(index(initdata))
        val (rcpu, rwall, a) = StopWatch.measureTime(initalizedindex.refresh())
        val attributes = List("refId", "indexId", "w", "avg_cpu", "avg_rcpu", "std_cpu", "std_rcpu")
        val initsummary = ExperimentSummary(attributes)
        initsummary.add("refId", generator.id + "-init")
        initsummary.add("indexId", initalizedindex.id)
        initsummary.add("w", windowsize)
        initsummary.add("avg_cpu", "%.6f".format(cpu))
        initsummary.add("avg_rcpu", "%.6f".format(rcpu))
        initsummary.add("std_cpu", 0)
        initsummary.add("std_rcpu", 0)
        //summary.add("rep", 0)
        initsummary.write(summaryPath)

        var measures: Array[Double] = Array()
        var rmeasures: Array[Double] = Array()
        for {
          n <- (0 until nrep)
        } {
          val newpoint: Double = dataset(windowsize + n)
          val (cpu, wall, a) = StopWatch.measureTime(initalizedindex.insert(newpoint))
          val (rcpu, rwall, b) = StopWatch.measureTime(initalizedindex.refresh())
          //val (rcpu, rwall, b) = StopWatch.measureTime({})
          measures = measures :+ cpu
          rmeasures = rmeasures :+ rcpu
        }
        //val attributes = List("refId", "indexId", "w", "avg_cpu", "avg_rcup", "std_cpu", "std_rcup")
        val summary = ExperimentSummary(attributes)
        summary.add("refId", generator.id)
        summary.add("indexId", initalizedindex.id)
        summary.add("w", windowsize)
        summary.add("avg_cpu", "%.6f".format(measures.sum / measures.length))
        //summary.add("avg_wall", "%.6f".format(wall))
        summary.add("avg_rcpu", "%.6f".format(rmeasures.sum / rmeasures.length))
        summary.add("std_cpu", "%.6f".format(breeze.stats.stddev(measures)))
        summary.add("std_rcpu", "%.6f".format(breeze.stats.stddev(rmeasures)))
        //summary.add("rwall", "%.6f".format(rwall))
        //summary.add("rep", n)
        summary.write(summaryPath)

        if (windowsize % 1000 == 0) {
          info(s"Avg ins cpu w=$windowsize: ${initalizedindex.id} -> " + "%.6f".format(measures.sum / measures.length))
          info(s"Avg ref cpu w=$windowsize: ${initalizedindex.id} -> " + "%.6f".format(rmeasures.sum / rmeasures.length))
        }
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}
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
object PerformanceContrast extends Experiment {
  val nrep = 100
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {
    info(s"Starting com.edouardfouche.experiments ${this.getClass.getSimpleName}")

    info(s"initialize indexes")
    val generators: Vector[DataGenerator] = Vector(
      Independent(3, 0, "gaussian", 10),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 10),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0)
      //Independent(1, 0, "gaussian", 0)
    )
    info(s"initialize generators")
    val tests: Vector[McdeStats] = Vector(
      CSPn(1, 0.5, 0.5),
      MWP(1, 0.5, 0.5),
      KSPn(1, 0.5, 0.5),
      KSPsn(1, 0.5, 0.5),
      CSPn(50, 0.5, 0.5),
      MWP(50, 0.5, 0.5),
      KSPn(50, 0.5, 0.5),
      KSPsn(50, 0.5, 0.5)
      //CSP(1, 0.5, 0.5),
      //MWPn(50, 0.5, 0.5),
    )
    info(s"initialize generators")

    //val datasets: Vector[(Array[Double], String)] = generators.map(x => (x.generate(200000).transpose.head, x.id))

    for {
      i <- tests.indices.par
    } {
      val generator = generators(i)
      val test: McdeStats = tests(i)
      //MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")
      info(s"Starting with test: ${test.id}")
      //val dataset = generator.generate(200000)

      for {n <- (1 to nrep).par} {
        for {windowsize <- ((100 until 100000) by 100)} runit(windowsize, n)
        info(s"${test.id}-${test.M}: Reached n=$n")
      }

      def runit(windowsize: Int, n: Int): Unit = {
        val initdata: DataSet = new DataSet(generator.generate(windowsize).transpose)
        val (prepcpu, prepwall, initalizedindex) = StopWatch.measureTime(test.preprocess(initdata))

        val (cpu, wall, contrast) = StopWatch.measureTime(test.contrast(initalizedindex, Set(0, 1, 2)))
        //cpumeasures = cpumeasures :+ cpu
        //prepmeasures = prepmeasures :+ prepcpu

        val attributes = List("refId", "testId", "M", "w", "cpu", "prepcpu", "rep")
        val summary = ExperimentSummary(attributes)
        summary.add("refId", generator.id)
        summary.add("testId", test.id)
        summary.add("M", test.M)
        summary.add("w", windowsize)
        summary.add("cpu", "%.6f".format(cpu))
        summary.add("prepcpu", "%.6f".format(prepcpu))
        //summary.add("rep", n)
        //summary.add("avg_wall", "%.6f".format(wall))
        //summary.add("avg_wall", "%.6f".format(wallmeasures.sum / wallmeasures.length))
        //summary.add("std_cpu", "%.6f".format(breeze.stats.stddev(cpumeasures)))
        //summary.add("std_wall", "%.6f".format(breeze.stats.stddev(wallmeasures)))
        //summary.add("rwall", "%.6f".format(rwall))
        summary.add("rep", n)
        summary.write(summaryPath)
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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
import io.github.edouardfouche.index._
import io.github.edouardfouche.index.dimension._
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch


/**
  * Created by fouchee on 12.07.17.
  * Test the influence of M on the scores
  */
object PerformanceIndex extends Experiment {
  val nrep = 100
  //override val data: Vector[DataRef] = Vector(Linear) // those are a selection of subspaces of different dimensionality and noise

  def run(): Unit = {
    info(s"Starting com.edouardfouche.experiments ${this.getClass.getSimpleName}")

    val indexes: Vector[DataSet => Index[DimensionIndex]] = Vector(
      new I_Count(_),
      new I_Count_Stream(_),
      new I_CRank(_),
      new I_CRank_Stream(_),
      new I_Rank(_),
      new I_Rank_Stream(_)
    )
    info(s"initialize indexes")
    val generators: Vector[DataGenerator] = Vector (
      Independent(3, 0, "gaussian", 10),
      Independent(3, 0, "gaussian", 10),
      //Independent(1, 0, "gaussian", 20),
      //Independent(1, 0, "gaussian", 20),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0),
      Independent(3, 0, "gaussian", 0)
    )
    info(s"initialize generators")

    //val datasets: Vector[(Array[Double], String)] = generators.map(x => (x.generate(200000).transpose.head, x.id))

    for {
      i <- indexes.indices.par
    } {
      val index = indexes(i)
      val generator = generators(i)
      //MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")
      val dummyindex = index(new DataSet(Array(Array(1, 2, 3))))

      // burn-in
      runit(100, 0)
      info(s"Starting with index: ${dummyindex.id}")

      for {n <- (1 to nrep).par} {
        for {windowsize <- ((100 to 100000) by 100)} runit(windowsize, n)
        info(s"${dummyindex.id}: Reached n=$n")
      }
      
      def runit(windowsize: Int, n: Int): Unit = {
        //var initmeasures: Array[Double] = Array()
        //var measures: Array[Double] = Array()
        //var rmeasures: Array[Double] = Array()

        val data = generator.generate(windowsize + 1) //.transpose.head
        val initdata: DataSet = new DataSet(data.transpose.map(x => x.take(windowsize)))
        val (initcpu, initwall, initalizedindex) = StopWatch.measureTime(index(initdata))

        val newpoint: Array[Double] = data(windowsize)
        val (cpu, wall, b) = StopWatch.measureTime(initalizedindex.insert(newpoint))
        val (rcpu, rwall, c) = StopWatch.measureTime(initalizedindex.refresh())
        //val (rcpu, rwall, b) = StopWatch.measureTime({})
        //initmeasures = initmeasures :+ initcpu
        //measures = measures :+ cpu
        //rmeasures = rmeasures :+ rcpu

        val attributes = List("refId", "indexId", "w", "initcpu", "cpu", "rcpu", "rep")
        val summary = ExperimentSummary(attributes)
        summary.add("refId", generator.id)
        summary.add("indexId", dummyindex.id)
        summary.add("w", windowsize)
        summary.add("initcpu", "%.6f".format(initcpu))
        summary.add("cpu", "%.6f".format(cpu))
        summary.add("rcpu", "%.6f".format(rcpu))
        //summary.add("rwall", "%.6f".format(rwall))
        summary.add("rep", n)
        summary.write(summaryPath)
        /*
        if (windowsize % 1000 == 0) {
          info(s"Avg ins cpu w=$windowsize: ${dummyindex.id} -> " + "%.6f".format(measures.sum / measures.length))
          info(s"Avg ref cpu w=$windowsize: ${dummyindex.id} -> " + "%.6f".format(rmeasures.sum / rmeasures.length))
        }
         */
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

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
object IndexPerfRandom extends Experiment {
  val nrep = 100000
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

    val windowsizes = Vector(100, 1000, 10000)

    val references = Vector(
      // the ordinal stuff
      //Independent(1,0,"gaussian",5),
      Independent(1, 0, "gaussian", 10),
      //Independent(1,0,"gaussian",20),

      // the numeric stuff
      Independent(1, 0, "gaussian", 0)

      // the categorical stuff
      //IndependentCat(1,0,"gaussian",5),
      // IndependentCat(1,0,"gaussian",10), // Basically same results as with ordinal
      //IndependentCat(1,0,"gaussian",20),
    )

    for {
      windowsize <- windowsizes
    } {

      for {
        reference <- references
      } {
        info(s"Starting with windowsize: $windowsize, reference: ${reference.id}")
        // initialize the indexes

        val fulldata: Array[Double] = reference.generate(windowsize + nrep).transpose.head
        val initdata: Array[Double] = fulldata.take(windowsize)

        val measures = scala.collection.mutable.Map[String, Double]()
        val rmeasures = scala.collection.mutable.Map[String, Double]()

        val indexes: Vector[DimensionIndex] = indexesconstructors.map(x => {
          val (cpu, wall, index) = StopWatch.measureTime(x(initdata))
          val (rcpu, rwall, a) = StopWatch.measureTime(index.refresh())
          val attributes = List("refId", "indexId", "w", "cpu", "wall", "rcup", "rwall", "rep")
          val summary = ExperimentSummary(attributes)
          summary.add("refId", reference.id)
          summary.add("indexId", index.id)
          summary.add("w", windowsize)
          summary.add("cpu", cpu)
          summary.add("wall", wall)
          summary.add("rcpu", rcpu)
          summary.add("rwall", rwall)
          summary.add("rep", 0)
          summary.write(summaryPath)
          measures(index.id) = cpu
          rmeasures(index.id) = rcpu
          index
        })

        for {
          n <- (1 to nrep)
        } {
          val newpoint: Double = fulldata(windowsize + n - 1)
          //if(newpoint.isNaN) println("generator produced a NaN !")
          for {
            i <- indexes
          } {
            val (cpu, wall, a) = StopWatch.measureTime(i.insert(newpoint))
            val (rcpu, rwall, b) = StopWatch.measureTime(i.refresh())
            //val (rcpu, rwall, b) = StopWatch.measureTime({})
            val attributes = List("refId", "indexId", "w", "cpu", "wall", "rcup", "rwall", "rep")
            val summary = ExperimentSummary(attributes)
            summary.add("refId", reference.id)
            summary.add("indexId", i.id)
            summary.add("w", windowsize)
            summary.add("cpu", cpu)
            summary.add("wall", wall)
            summary.add("rcpu", rcpu)
            summary.add("rwall", rwall)
            summary.add("rep", n)
            summary.write(summaryPath)
            measures(i.id) = measures(i.id) + cpu
            rmeasures(i.id) = rmeasures(i.id) + rcpu
          }
        }
        info(s"Total insert  cpu time (ms): ${measures.mkString(",")}")
        info(s"Total refresh cpu time (ms): ${rmeasures.mkString(",")}")
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}

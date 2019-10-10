package io.github.edouardfouche.mcde

import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec

case class StreamEstimator(test: McdeStats, windowsize: Int, stepsize: Int, gamma: Double, stream: Boolean) {

  val id = s"${test.id}-${test.M}-${test.parallelize}||${windowsize}-${stepsize}-${gamma}-${if (stream) 1 else 0}"

  def run(data: DataSet): Array[Double] = {
    val initdataset = data.take(windowsize)
    val restdataset = data.drop(windowsize)
    val index: test.I = test.preprocess(initdataset, stream = stream)
    val initcontrast = test.contrast(index, (0 until data.ncols).toSet)


    @tailrec
    def cumulative_contrast(data: DataSet, listofcontrast: List[Double], acc: Int): Array[Double] = {
      if (data.nrows == 0) listofcontrast.reverse.toArray
      else {
        if (acc % stepsize == 0) {
          index.insert(data.head)
          val newcontrast = test.contrast(index, (0 until data.ncols).toSet)
          cumulative_contrast(data.tail, listofcontrast.head * (gamma) + newcontrast * (1 - gamma) :: listofcontrast, acc + 1)
        } else {
          index.insert(data.head)
          cumulative_contrast(data.tail, listofcontrast.head :: listofcontrast, acc + 1)
        }
      }
    }

    cumulative_contrast(restdataset, List(initcontrast), 0)
  }

}

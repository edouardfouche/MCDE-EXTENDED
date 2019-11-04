package io.github.edouardfouche.mcde

import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec

case class StreamEstimator(test: McdeStats, windowsize: Int, stepsize: Int, gamma: Double, stream: Boolean) {

  val id = s"${test.id}-${test.M}-${test.parallelize}||${windowsize}-${stepsize}-${gamma}-${if (stream) 1 else 0}"
  val formatter = new java.text.SimpleDateFormat("dd-HH:mm")

  def runMultiple(data: DataSet, subspaces: Array[List[Int]]): scala.collection.mutable.Map[List[Int], Array[Double]] = {
    val initdataset = data.take(windowsize)
    val restdataset = data.drop(windowsize)
    val index: test.I = test.preprocess(initdataset, stream = stream)

    val initcontrasts = scala.collection.mutable.Map(subspaces.map(x => x -> List(test.contrast(index, x.toSet))): _*)

    @tailrec
    def cumulative_contrast(data: DataSet,
                            contrasts: scala.collection.mutable.Map[List[Int], List[Double]],
                            acc: Int): scala.collection.mutable.Map[List[Int], Array[Double]] = {
      if (data.nrows == 0) contrasts.map(x => x._1 -> x._2.reverse.toArray) //listofcontrast.reverse.toArray
      else {
        if (acc % 1000 == 0) println(s"${formatter.format(java.util.Calendar.getInstance().getTime)}, $id : reached $acc")
        if (acc % stepsize == 0) {
          //println("insert with contrast computation")
          index.insert(data.head)
          val newcontrasts = contrasts.keys.map(x => x -> test.contrast(index, x.toSet)).toMap
          for {sub <- contrasts.keys} {
            contrasts(sub) = contrasts(sub).head * (gamma) + newcontrasts(sub) * (1 - gamma) :: contrasts(sub)
          }
          cumulative_contrast(data.tail, contrasts, acc + 1)
        } else {
          //println("insert without contrast computation")
          index.insert(data.head)
          for {sub <- contrasts.keys} {
            contrasts(sub) = contrasts(sub).head :: contrasts(sub)
          }
          cumulative_contrast(data.tail, contrasts, acc + 1)
        }
      }
    }

    cumulative_contrast(restdataset, initcontrasts, 0)
  }


  def run(data: DataSet): Array[Double] = {
    //println("initialize estimator")
    val initdataset = data.take(windowsize)
    val restdataset = data.drop(windowsize)
    val index: test.I = test.preprocess(initdataset, stream = stream)
    val initcontrast = test.contrast(index, (0 until data.ncols).toSet)

    @tailrec
    def cumulative_contrast(data: DataSet, listofcontrast: List[Double], acc: Int): Array[Double] = {
      if (data.nrows == 0) listofcontrast.reverse.toArray
      else {
        if (acc % 10000 == 0) println(s"$id : reached $acc")
        if (acc % stepsize == 0) {
          //println("insert with contrast computation")
          index.insert(data.head)
          val newcontrast = test.contrast(index, (0 until data.ncols).toSet)
          cumulative_contrast(data.tail, listofcontrast.head * (gamma) + newcontrast * (1 - gamma) :: listofcontrast, acc + 1)
        } else {
          //println("insert without contrast computation")
          index.insert(data.head)
          cumulative_contrast(data.tail, listofcontrast.head :: listofcontrast, acc + 1)
        }
      }
    }

    cumulative_contrast(restdataset, List(initcontrast), 0)
  }

}

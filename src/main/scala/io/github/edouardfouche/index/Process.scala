package io.github.edouardfouche.index

import io.github.edouardfouche.index.tuple.{AdjustedRankTupleIndex, CorrectedRankTupleIndex, RankTupleIndex}

import scala.collection.parallel.ForkJoinTaskSupport

object Process {
  /**
    * Return the rank index structure for MWP, with adjusted ranks but no correction for ties.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of 2-D Tuple, where the first element is the original index, the second is its rank.
    */
  def mwRank(input: Array[Double]): Array[AdjustedRankTupleIndex] = {
    // Create an index for a column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))

    val m = nonadjusted.length - 1
    var j = 0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._3 == nonadjusted(k + 1)._3)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._2
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._2) / (k - j + 1.0)).toFloat
        (j to k).foreach(y => nonadjusted(y) = (nonadjusted(y)._1, newval, nonadjusted(y)._3))
        j += k - j + 1 // jump to after the replacement
      } else j += 1
    }
    nonadjusted.map(y => AdjustedRankTupleIndex(y._1,y._2))
  }

  /**
    * Return the rank index structure for MWP, with adjusted ranks AND correction for ties.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of 3-D Tuple, where the first element is the original index, the second is its rank and the
    *         the last one a cumulative correction for ties.
    */
  def mwRankCorrectionCumulative(input: Array[Double]): Array[CorrectedRankTupleIndex] = {
    // Create an index for each column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._2.toFloat, y._1._1))

    val m = nonadjusted.length - 1
    var j = 0
    var acc_corr = 0.0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._3 == nonadjusted(k + 1)._3)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._2
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._2) / (k - j + 1.0)).toFloat
        val t = k - j + 1.0
        acc_corr = acc_corr + math.pow(t , 3) - t
        (j to k).foreach(y => nonadjusted(y) = (nonadjusted(y)._1, newval, acc_corr))
        j += k - j + 1 // jump to after the replacement
      } else {
        nonadjusted(j) = (nonadjusted(j)._1, nonadjusted(j)._2, acc_corr)
        j += 1
      }
    }

    nonadjusted.map(x => CorrectedRankTupleIndex(x))
  }

  /**
    * Return the rank index structure (as in HiCS).
    *
    * Note that the numbers might be different in the case of ties, in comparison with other implementations.
    *
    * @param input A 2-D Array of Double (data set, column-oriented).
    * @return A 2-D Array of Int, where the element is the original index in the unsorted data set
    */
  def ksRankSimple(input: Array[Double]): Array[RankTupleIndex] = {
    input.zipWithIndex.sortBy(_._1).map(x => RankTupleIndex(x._2))
  }
}

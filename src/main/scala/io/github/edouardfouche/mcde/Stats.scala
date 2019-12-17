package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.Index
import io.github.edouardfouche.index.dimension.DimensionIndex
import io.github.edouardfouche.preprocess.DataSet

/**
  * Created by fouchee on 07.07.17.
  */
abstract class Stats{
  type D <: DimensionIndex
  type I <: Index[D]
  val id: String
  val alpha: Double
  val beta: Double
  val M: Int

  /**
    * @param input A data set (row oriented)
   */
  def preprocess(input: DataSet, stream: Boolean = false): I

  /**
    * @param m A data set (row oriented)
    */
  def contrast(m: DataSet, dimensions: Set[Int]): Double = {
    this.contrast(this.preprocess(m), dimensions)
  }

  def contrast(m: I, dimensions: Set[Int]): Double

  /**
    * @param m A data set (row oriented)
    */
  def contrastMatrix(m: DataSet): Array[Array[Double]] = {
    this.contrastMatrix(this.preprocess(m))
  }

  def contrastMatrix(m: I): Array[Array[Double]]
}

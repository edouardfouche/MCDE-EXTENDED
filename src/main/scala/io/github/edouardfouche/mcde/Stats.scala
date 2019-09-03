package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.{DoubleIndex, Index}

/**
  * Created by fouchee on 07.07.17.
  */
abstract class Stats{
  //type U = Double with Int
  //type PreprocessedData = DoubleIndex //<: Index[Double] // PreprocessedData are subtypes of Index, which are column oriented structures
  //type U = _ <: Ordered[U]
  // type PreprocessedData = _ <: Index[U]

  val id: String
  val alpha: Double
  val beta: Double
  val M: Int

  /**
    * @param input A data set (row oriented)
   */
  def preprocess(input: Array[Array[Double]]): Index[Double]

  /**
    * @param m A data set (row oriented)
    */
  def contrast(m: Array[Array[Double]], dimensions: Set[Int]): Double = {
    this.contrast(this.preprocess(m), dimensions)
  }

  def contrast(m: Index[Double], dimensions: Set[Int]): Double

  /**
    * @param m A data set (row oriented)
    */
  def contrastMatrix(m: Array[Array[Double]]): Array[Array[Double]] = {
    this.contrastMatrix(this.preprocess(m))
  }

  def contrastMatrix(m: Index[Double]): Array[Array[Double]]
}

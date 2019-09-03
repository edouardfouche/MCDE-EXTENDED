package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.{Index_Double, Index}

/**
  * Created by fouchee on 07.07.17.
  */
abstract class Stats{
  //type U = Double with Int
  //type PreprocessedData = Index_Double //<: Index[Double] // PreprocessedData are subtypes of Index, which are column oriented structures
  //type U = _ <: Ordered[U]
  // type PreprocessedData = _ <: Index[U]

  val id: String
  val alpha: Double
  val beta: Double
  val M: Int

  /**
    * @param input A data set (row oriented)
   */
  def preprocess[U](input: Array[Array[U]])(implicit ev$1: U => Ordered[U]): Index[U]

  /**
    * @param m A data set (row oriented)
    */
  def contrast[U](m: Array[Array[U]], dimensions: Set[Int])(implicit ev$1: U => Ordered[U]): Double = {
    this.contrast(this.preprocess(m), dimensions)
  }

  def contrast[U](m: Index[U], dimensions: Set[Int])(implicit ev$1: U => Ordered[U]): Double

  /**
    * @param m A data set (row oriented)
    */
  def contrastMatrix[U](m: Array[Array[U]])(implicit ev$1: U => Ordered[U]): Array[Array[Double]] = {
    this.contrastMatrix(this.preprocess(m))
  }

  def contrastMatrix[U](m: Index[U])(implicit ev$1: U => Ordered[U]): Array[Array[Double]]
}

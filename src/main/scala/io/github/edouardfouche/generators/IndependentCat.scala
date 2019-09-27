package io.github.edouardfouche.generators

import breeze.stats.distributions.Uniform

case class IndependentCat(nDim: Int, noise: Double, noisetype: String, discretize: Int) extends DataGenerator {
  val name = "independentcat"
  override lazy val shortname = "ic"

  def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { _ =>
      (1 to nDim).toArray.map(_ => Uniform(0, 1).draw())
    }
  }

  override def generate(n: Int): Array[Array[Double]] = {
    Categorizer.categorize(postprocess(getPoints(n: Int), noise, noisetype), discretize)
  }
}
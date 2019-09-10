package io.github.edouardfouche.generators

import breeze.stats.distributions.Uniform

case class LinearCat(nDim: Int, noise: Double, noisetype: String, discretize: Int) extends DataGenerator {
  val name = "linearcat"

  def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { _ =>
      val x = Uniform(0, 1).draw()
      (1 to nDim).toArray.map(_ => x)
    }
  }

  override def generate(n: Int): Array[Array[Double]] = {
    Categorizer.categorize(postprocess(getPoints(n: Int), noise, noisetype), discretize)
  }
}
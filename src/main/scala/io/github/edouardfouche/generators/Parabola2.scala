package io.github.edouardfouche.generators

import breeze.stats.distributions.Uniform

// This generator fixes a bug in the Parabola implementation of  the io.github.edouardfouche.generators package
case class Parabola2(nDim: Int, noise: Double, noisetype: String, discretize: Int)(scale: Option[Double] = Some(1)) extends ParameterizedDataGenerator {
  override lazy val shortname = "P2"
  val s: Int = scale match {
    case Some(i) => i.asInstanceOf[Int]
    case None => 1
  }
  val param: Double = s
  val name = "parabola2"

  protected def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { _ =>
      var data = Array(Uniform(-1, 1).draw())
      for (y <- 2 to nDim) {
        data = data ++ Array(math.pow(data(0), 2 + (2 * (s - 1))))
      }
      data.map(x => noiselessPowerNormalization(x, 2 + (2 * (s - 1))))
    }
  }
}

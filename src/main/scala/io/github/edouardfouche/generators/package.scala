package io.github.edouardfouche

import breeze.stats.distributions.Gaussian

import scala.annotation.tailrec

package object generators {
  @tailrec
  def gaussianprocess(start: Double, current: Array[Double], n: Int): Array[Double] = {
    if (n == 0) return current
    val newval: Double = start + Gaussian(0, 0.05).draw()

    val inboundval: Double = if ((newval <= 1) && (newval >= 0)) {
      newval
    } else if (newval < 0) {
      -newval
    } else {
      1 - (newval - 1)
    }
    gaussianprocess(inboundval, current :+ inboundval, n - 1)
  }
}

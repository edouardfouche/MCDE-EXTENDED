package io.github.edouardfouche.generators

object Categorizer {
  // expect rows
  def categorize(data: Array[Array[Double]], nLevels: Int): Array[Array[Double]] = {
    if(nLevels == 0) data
    else
      data.transpose.map(y => {
        // This part is just like the discretizer
        val minVal = y.min
        val maxVal = y.max
        val amplitude = maxVal - minVal

        val discrete = y.map(x => {
          if (nLevels == 1) amplitude / 2.0
          else if (math.abs(x % (amplitude / (nLevels - 1))) > math.abs(x % (-amplitude / (nLevels - 1)))) x - (x % (-amplitude / (nLevels - 1)))
          else x - (x % (amplitude / (nLevels - 1)))
        })

        // Here we just take a random permutation
        val values = discrete.distinct
        val code = scala.util.Random.shuffle(values.indices.toList)
        val mapping: Map[Double, Int] = values.zip(code).toMap

        discrete.map(x => mapping(x).toDouble)
      }).transpose
  }

}

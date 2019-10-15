import io.github.edouardfouche.mcde.MWP
import io.github.edouardfouche.preprocess.DataSet

val len = 100
val bound = 1000
val random = scala.util.Random

val a1: Array[Double] = (for (i <- 0 until len) yield 1.0).toArray // random.nextInt(bound).toDouble).toArray
val a2: Array[Double] = (for (i <- 0 until len) yield 1.0).toArray // random.nextInt(bound).toDouble).toArray
val data: Array[Array[Double]] = Array(a1, a2)
val alpha = 0.5 // 0.1 + 0.9 * random.nextDouble();
val beta = 0.5 // 0.1 + 0.9 * random.nextDouble();
val mwp = MWP(M = 1000, alpha = alpha, beta = beta)
mwp.contrast(new DataSet(data), Set(0, 1))

import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._

val ddd = new DataSet(Independent(3, 0.0, "gaussian", 3).generate(1000).transpose)

val m = 1

CSP(100).contrast(ddd, Set(0, 1))

(1 to 100).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 3).generate(1000).transpose), Set(0, 1))).sum / 100

val dddd = new DataSet(Independent(3, 0.0, "gaussian", 3).generate(1000).transpose)

CSP(100).contrast(dddd, Set(0, 1))

(1 to 100).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / 100

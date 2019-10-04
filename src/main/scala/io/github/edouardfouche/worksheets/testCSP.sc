import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._

val m = 1

val c = new DataSet(Independent(3, 0, "gaussian", 20).generate(1000).transpose)
c.columns(0).groupBy(identity).mapValues(_.length)

(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 20).generate(1000).transpose), Set(0, 1))).sum / 1000


val d = new DataSet(Independent(3, 0, "gaussian", 10).generate(1000).transpose)
d.columns(0).groupBy(identity).mapValues(_.length)

(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 10).generate(1000).transpose), Set(0, 1))).sum / 1000

val dd = new DataSet(Independent(3, 0, "gaussian", 5).generate(1000).transpose)

(1 to 1000).toArray.map(x =>
  MWP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / 1000
(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / 1000

val ddd = new DataSet(Independent(3, 0.0, "gaussian", 3).generate(1000).transpose)

CSP(1000).contrast(ddd, Set(0, 1))
MWP(1000).contrast(ddd, Set(0, 1))
KSP(10).contrast(ddd, Set(0, 1))

val ddd2 = new DataSet(Independent(3, 0.0, "gaussian", 2).generate(1000).transpose)

CSP(1000).contrast(ddd2, Set(0, 1))
MWP(1000).contrast(ddd2, Set(0, 1))
KSP(10).contrast(ddd2, Set(0, 1))

val ddd3 = new DataSet(Independent(3, 0.0, "gaussian", 1).generate(1000).transpose)

CSP(1000).contrast(ddd3, Set(0, 1))
MWP(1000).contrast(ddd3, Set(0, 1))
KSP(10).contrast(ddd3, Set(0, 1, 2))

val ddd4 = new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose)

CSPn(1000).contrast(ddd4, Set(0, 1))
MWPn(1000).contrast(ddd4, Set(0, 1))
KSPn(10).contrast(ddd4, Set(0, 1, 2))

CSP(1000).contrast(ddd4, Set(0, 1))
MWP(1000).contrast(ddd4, Set(0, 1))
KSP(10).contrast(ddd4, Set(0, 1, 2))


(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 3).generate(1000).transpose), Set(0, 1))).sum / 1000

val dddd = new DataSet(Independent(3, 0.0, "gaussian", 2).generate(1000).transpose,
  types = Array("c", "o", "n"))

(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / 1000


val ddddd = new DataSet(Linear(5, 0.0, "gaussian", 10).generate(1000).transpose,
  types = Array("c", "o", "n"))

(1 to 1000).toArray.map(x =>
  CSP(m).contrast(new DataSet(Linear(3, 0, "gaussian", 10).generate(1000).transpose), Set(0, 1))).sum / 1000
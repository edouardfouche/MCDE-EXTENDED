import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._

val m = 10
val n = 1

val c = new DataSet(Independent(3, 0, "gaussian", 20).generate(1000).transpose)
c.columns(0).groupBy(identity).mapValues(_.length)

(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 20).generate(1000).transpose), Set(0, 1))).sum / n


val d = new DataSet(Independent(3, 0, "gaussian", 10).generate(1000).transpose)
d.columns(0).groupBy(identity).mapValues(_.length)

(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 10).generate(1000).transpose), Set(0, 1))).sum / n

val dd = new DataSet(Independent(3, 0, "gaussian", 5).generate(1000).transpose)

(1 to n).toArray.map(x =>
  MWP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / n
(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / n

val ddd = new DataSet(Independent(3, 0.0, "gaussian", 3).generate(1000).transpose)

CSP(m).contrast(ddd, Set(0, 1))
MWP(m).contrast(ddd, Set(0, 1))
MWPu(m).contrast(ddd, Set(0, 1))
KSP(m).contrast(ddd, Set(0, 1))

val ddd2 = new DataSet(Independent(3, 0.0, "gaussian", 2).generate(1000).transpose)

CSPn(m).contrast(ddd2, Set(0, 1))
//MWPn(1000).contrast(ddd2, Set(0, 1))
KSPn(m).contrast(ddd2, Set(0, 1))

CSP(m).contrast(ddd2, Set(0, 1))
MWP(m).contrast(ddd2, Set(0, 1))
MWPu(m).contrast(ddd2, Set(0, 1))
KSP(m).contrast(ddd2, Set(0, 1))

val ddd3 = new DataSet(Independent(3, 0.0, "gaussian", 1).generate(1000).transpose)

CSP(m).contrast(ddd3, Set(0, 1))
MWP(m).contrast(ddd3, Set(0, 1))
MWPu(m).contrast(ddd3, Set(0, 1))
KSP(m).contrast(ddd3, Set(0, 1))

val ddd33 = new DataSet(Linear(3, 0.0, "gaussian", 1).generate(1000).transpose)

CSPn(m).contrast(ddd33, Set(0, 1))
//MWPn(1000).contrast(ddd33, Set(0, 1))
KSPn(m).contrast(ddd33, Set(0, 1))

CSP(m).contrast(ddd33, Set(0, 1))
MWP(m).contrast(ddd33, Set(0, 1))
MWPu(m).contrast(ddd33, Set(0, 1))
KSP(m).contrast(ddd33, Set(0, 1))

val ddd4 = new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose)

CSPn(m).contrast(ddd4, Set(0, 1))
//MWPn(1000).contrast(ddd4, Set(0, 1))
KSPn(m).contrast(ddd4, Set(0, 1))

CSP(m).contrast(ddd4, Set(0, 1))
MWP(m).contrast(ddd4, Set(0, 1))
MWPu(m).contrast(ddd4, Set(0, 1))
KSP(m).contrast(ddd4, Set(0, 1))

val ddd5 = new DataSet(Linear(20, 0.0, "gaussian", 0).generate(1000).transpose)

MWP(m).contrast(ddd5, (0 until 20).toSet)
MWPn(m).contrast(ddd5, (0 until 20).toSet)
MWPu(m).contrast(ddd5, (0 until 20).toSet)
KSPn(m).contrast(ddd5, (0 until 20).toSet)

val ddd55 = new DataSet(Linear(3, 0.0, "gaussian", 0).generate(1000).transpose)

MWPn(m).contrast(ddd55, (0 until 3).toSet)
MWPu(m).contrast(ddd55, (0 until 3).toSet)
KSPn(m).contrast(ddd55, (0 until 3).toSet)

val ddd6 = new DataSet(Independent(20, 0.0, "gaussian", 0).generate(1000).transpose)

MWP(m).contrast(ddd6, (0 until 20).toSet)
MWPn(m).contrast(ddd6, (0 until 20).toSet)
MWPu(m).contrast(ddd6, (0 until 20).toSet)
KSPn(m).contrast(ddd6, (0 until 20).toSet)


(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 3).generate(1000).transpose), Set(0, 1))).sum / n

val dddd = new DataSet(Independent(3, 0.0, "gaussian", 2).generate(1000).transpose,
  types = Array("c", "o", "n"))

(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Independent(3, 0, "gaussian", 2).generate(1000).transpose), Set(0, 1))).sum / n


val ddddd = new DataSet(Linear(5, 0.0, "gaussian", 10).generate(1000).transpose,
  types = Array("c", "o", "n"))

(1 to n).toArray.map(x =>
  CSP(m).contrast(new DataSet(Linear(3, 0, "gaussian", 10).generate(1000).transpose), Set(0, 1))).sum / n
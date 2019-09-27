import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils.StopWatch

val data = MWP().preprocess(new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose))

val d = new DataSet(Independent(3, 0, "gaussian", 0).generate(1000).transpose,
  types = Array("c", "c", "c"))

d(0).length
d(0).distinct.length

StopWatch.measureTime { // having some problem here
  CSP(10).contrast(d, Set(0, 1))
}

val dd = new DataSet(Independent(3, 0, "gaussian", 5).generate(1000).transpose,
  types = Array("o", "o", "o"))

dd(0).length
dd(0).distinct.length

StopWatch.measureTime { // having some problem here
  CSP(10).contrast(dd, Set(0, 1))
}

val ddd = new DataSet(Linear(3, 0.0, "gaussian", 0).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  CSP(10).contrast(ddd, Set(0, 1, 2))
}

val dddd = new DataSet(Linear(3, 0.0, "gaussian", 20).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  CSP(10).contrast(dddd, Set(0, 1, 2))
}

val ddddd = new DataSet(Linear(5, 0.0, "gaussian", 10).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  CSP(10).contrast(ddddd, Set(0, 1, 2, 3, 4))
}
import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils.StopWatch

val data = MWP().preprocess(new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose))

val d = new DataSet(Independent(3, 0, "gaussian", 0).generate(1000).transpose,
  types = Array("c", "c", "c"))

d(0).length
d(0).distinct.length

StopWatch.measureTime {
  MWP().contrast(d, Set(0, 1))
}
StopWatch.measureTime { // having some problem here
  KSP(200).contrast(d, Set(0, 1))
}
StopWatch.measureTime { // having some problem here
  KSPP(200).contrast(d, Set(0, 1))
}
StopWatch.measureTime {
  CSP(500).contrast(d, Set(0, 1))
}

val dd = new DataSet(Independent(3, 0, "gaussian", 5).generate(1000).transpose,
  types = Array("o", "o", "o"))

dd(0).length
dd(0).distinct.length

StopWatch.measureTime {
  MWP().contrast(dd, Set(0, 1))
}
StopWatch.measureTime { // having some problem here
  KSP(200).contrast(dd, Set(0, 1))
}
StopWatch.measureTime { // having some problem here
  KSPP(200).contrast(dd, Set(0, 1))
}
StopWatch.measureTime {
  CSP(500).contrast(dd, Set(0, 1))
}

val ddd = new DataSet(Linear(3, 0.0, "gaussian", 0).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  MWP().contrast(ddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  KSP().contrast(ddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  KSPP().contrast(ddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  CSP(100).contrast(ddd, Set(0, 1, 2))
}

val dddd = new DataSet(Linear(3, 0.0, "gaussian", 20).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  MWP().contrast(dddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  KSP().contrast(dddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  KSPP().contrast(dddd, Set(0, 1, 2))
}
StopWatch.measureTime {
  CSP(100).contrast(dddd, Set(0, 1, 2))
}

val ddddd = new DataSet(Linear(5, 0.0, "gaussian", 0).generate(1000).transpose,
  types = Array("c", "o", "n"))

StopWatch.measureTime {
  MWP().contrast(ddddd, Set(0, 1, 2, 3, 4))
}
StopWatch.measureTime {
  KSP().contrast(ddddd, Set(0, 1, 2, 3, 4))
}
StopWatch.measureTime {
  KSPP().contrast(ddddd, Set(0, 1, 2, 3, 4))
}
StopWatch.measureTime {
  CSP(100).contrast(ddddd, Set(0, 1, 2, 3, 4))
}
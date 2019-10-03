import io.github.edouardfouche.generators._
import io.github.edouardfouche.index._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils.StopWatch

val data = MWP().preprocess(new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose))

data.ncols

data.nrows


val test = MWP()
val result = test.contrastMatrix(data)

val d1 = new DataSet(Array(Array(1,3,2), Array(4,6,5), Array(8,9,7)))
new I_CRank(d1)
val d2 = new DataSet(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)),
  Array("c", "c", "c"))
new I_CRank(d2)

val d3 = new DataSet(Array(Array(1, 2, 3), Array(2.0, 3.0, 4.0), Array(5, 6, 7)),
  Array("c", "n", "o"))
val i = new I_CRank(d3)
i.data(0)
i.data(1)
i.data(2)

val ii = new I_Multi(d3)
ii.data(0)
ii.data(1)
ii.data(2)
ii(0)
ii(1)
ii(2)

val d = new DataSet(Independent(3, 0, "gaussian", 0).generate(1000).transpose,
  types=Array("c", "c", "c"))

d(0).length
d(0).distinct.length

StopWatch.measureTime{
  MWP().contrast(d, Set(0,1))
}
StopWatch.measureTime{ // having some problem here
  KSP_bis(200).contrast(d, Set(0, 1))
}
StopWatch.measureTime{ // having some problem here
  KSP_bisn(200).contrast(d, Set(0, 1))
}
StopWatch.measureTime{ // having some problem here
  KSP(200).contrast(d, Set(0, 1))
}

StopWatch.measureTime{
  CSP(500).contrast(d, Set(0,1))
}

StopWatch.measureTime{
  MCDEP(500).contrast(d, Set(0,1,2))
}

val dd = new DataSet(Linear(3, 0.0, "gaussian", 0).generate(1000).transpose,
  types=Array("c", "o", "n"))

StopWatch.measureTime{
  MWP().contrast(dd, Set(0,1,2))
}
StopWatch.measureTime{
  KSP_bis().contrast(dd, Set(0, 1, 2))
}
StopWatch.measureTime{
  KSP().contrast(dd, Set(0, 1, 2))
}

StopWatch.measureTime{
  CSP(100).contrast(dd, Set(0,1,2))
}

StopWatch.measureTime{
  MCDEP(100).contrast(dd, Set(0,1,2))
}

print("lol")
StopWatch.measureTime{
  KSP_bis().get_p_from_D(0.1, 500, 500)
}
StopWatch.measureTime {
  KSP().get_p_from_D(0.1, 500, 500)
}

StopWatch.measureTime {
  KSP_bis().get_p_from_D(0.1, 1000, 1000)
  KSP_bis().get_p_from_D(0.05, 1000, 1000)
  KSP_bis().get_p_from_D(0.05, 800, 1000)
  KSP_bis().get_p_from_D(0.05, 1000, 100)
  KSP_bis().get_p_from_D(0.01, 100, 1000)
  KSP_bis().get_p_from_D(0.01, 1000, 100)
}


StopWatch.measureTime {
  KSP().get_p_from_D(0.1, 1000, 1000)
  KSP().get_p_from_D(0.05, 1000, 1000)
  KSP().get_p_from_D(0.05, 800, 1000)
  KSP().get_p_from_D(0.05, 1000, 100)
  KSP().get_p_from_D(0.01, 100, 1000)
  KSP().get_p_from_D(0.01, 1000, 100)
}

StopWatch.measureTime {
  KSP_bis().get_p_from_D(0.1, 1000, 1000)
}
StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 1000, 1000)
}

StopWatch.measureTime{
  KSP_bis().get_p_from_D(0.1, 500, 500)
}
StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 500, 500)
}

StopWatch.measureTime{
  KSP_bis().get_p_from_D(0.1, 100, 100)
}
StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 100, 100)
}
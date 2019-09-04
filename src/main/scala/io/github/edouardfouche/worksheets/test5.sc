import io.github.edouardfouche.generators._
import io.github.edouardfouche.index.Index_CorrectedRank
import io.github.edouardfouche.mcde.{KSP, KSPP, MWP}
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils.StopWatch

val data = MWP().preprocess(new DataSet(Independent(3, 0.0, "gaussian", 0).generate(1000).transpose))

data.ncols

data.nrows


val test = MWP(50, parallelize = 0)
val result = test.contrastMatrix(data)

val d1 = new DataSet(Array(Array(1,3,2), Array(4,6,5), Array(8,9,7)))
new Index_CorrectedRank(d1)
val d2 = new DataSet(Array(Array("a", "b", "c"), Array("c", "e", "a"), Array("a", "x", "z")))
new Index_CorrectedRank(d2)

val d3 = new DataSet(Array(Array("a", "b", "c"), Array(1.0, 2.0, 3.0), Array(1, 2, 3)))
val i = new Index_CorrectedRank(d3)
i.data(0)
i.data(1)
i.data(2)

val d = new DataSet(Independent(3, 0.6, "gaussian", 0).generate(1000).transpose)
val dp = MWP().preprocess(d)
StopWatch.measureTime{
  MWP().preprocess(d)
}
StopWatch.measureTime{
  MWP().contrast(dp, Set(0,1,2))
}

StopWatch.measureTime{
  KSP().preprocess(d)
}
StopWatch.measureTime{
  KSP().contrast(dp, Set(0,1,2))
}

StopWatch.measureTime{
  KSPP().preprocess(d)
}
StopWatch.measureTime{
  KSPP().contrast(dp, Set(0,1,2))
}

print("lol")
StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 500, 500)
}
StopWatch.measureTime {
  KSPP().get_p_from_D(0.1, 500, 500)
}

StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 1000, 1000)
  KSP().get_p_from_D(0.05, 1000, 1000)
  KSP().get_p_from_D(0.05, 800, 1000)
  KSP().get_p_from_D(0.05, 1000, 100)
  KSP().get_p_from_D(0.01, 100, 1000)
  KSP().get_p_from_D(0.01, 1000, 100)
}


StopWatch.measureTime {
  KSPP().get_p_from_D(0.1, 1000, 1000)
  KSPP().get_p_from_D(0.05, 1000, 1000)
  KSPP().get_p_from_D(0.05, 800, 1000)
  KSPP().get_p_from_D(0.05, 1000, 100)
  KSPP().get_p_from_D(0.01, 100, 1000)
  KSPP().get_p_from_D(0.01, 1000, 100)
}

StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 1000, 1000)
}
StopWatch.measureTime{
  KSPP().get_p_from_D(0.1, 1000, 1000)
}

StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 500, 500)
}
StopWatch.measureTime{
  KSPP().get_p_from_D(0.1, 500, 500)
}

StopWatch.measureTime{
  KSP().get_p_from_D(0.1, 100, 100)
}
StopWatch.measureTime{
  KSPP().get_p_from_D(0.1, 100, 100)
}
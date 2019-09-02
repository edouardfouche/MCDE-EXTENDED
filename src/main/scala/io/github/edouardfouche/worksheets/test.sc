import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde.{MWP,KSP,KSPP}
import io.github.edouardfouche.utils.StopWatch

val data = MWP().preprocess(Independent(3, 0.0, "gaussian", 0).generate(1000))

data.values.length

data.values(0).length

data.numCols

val test = MWP(50, parallelize = 0)
val result = test.contrastMatrix(data)

result


val d = Independent(3, 0.6, "gaussian", 0).generate(1000)
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
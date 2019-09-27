
import io.github.edouardfouche.utils.StopWatch

val a = scala.collection.immutable.Queue[Int]((1 to 1000000).toArray: _*)
//val a = ListBuffer((1 to 1000000).toArray:_*)

val b = (1 to 1000000).toArray

StopWatch.measureCPUTime(a.map(x => x + 1))

StopWatch.measureCPUTime(b.map(x => x + 1))
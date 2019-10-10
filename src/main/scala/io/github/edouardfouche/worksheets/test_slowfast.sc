import io.github.edouardfouche.generators._

val ndim = 2
val a = generateSmoothSlopUp
val slowchanging: Array[Array[Double]] = (generateSmoothSlopUp ++ generateSmoothSlopDown ++
  generateSmoothSlopUp ++ generateSmoothSlopDown ++
  generateSmoothSlopUp ++ generateSmoothSlopDown ++
  generateSmoothSlopUp ++ generateSmoothSlopDown ++
  generateSmoothSlopUp ++ generateSmoothSlopDown).transpose
val fastchanging: Array[Array[Double]] = (generateAbruptSlopUp ++ generateAbruptSlopDown ++
  generateAbruptSlopUp ++ generateAbruptSlopDown ++
  generateAbruptSlopUp ++ generateAbruptSlopDown ++
  generateAbruptSlopUp ++ generateAbruptSlopDown ++
  generateAbruptSlopUp ++ generateAbruptSlopDown).transpose

def generateSmoothSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray

def generateSmoothSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x / 100.0, "gaussian", 0).generate(1000)).toArray
a.length
a(0).length

def generateAbruptSlopUp: Array[Array[Double]] = (0 until 100).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray

def generateAbruptSlopDown: Array[Array[Double]] = (100 until 0).flatMap(x => Linear(ndim, x % 2, "gaussian", 0).generate(1000)).toArray

fastchanging.length
fastchanging(0).length
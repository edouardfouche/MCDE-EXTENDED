
import io.github.edouardfouche.generators._
import io.github.edouardfouche.mcde.MWP
import io.github.edouardfouche.preprocess.DataSet

val gen = Parabola(20, 0.0, "gaussian", 0)(scale = Some(1))

val generated_data = gen.generate(1000).transpose
val data = new DataSet(generated_data.map(x => x.map(y => if (y.isNaN) 0.0 else y)))

data.nrows
data.ncols
(0 until 20).flatMap(x => data(x)).distinct

data(0).distinct
MWP(50).preprocess(data)

import io.github.edouardfouche.generators._
import io.github.edouardfouche.index._
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess._

val c = new DataSet(Independent(3, 0, "gaussian", 20).generate(1000).transpose)

val odd = new I_Rank_Stream(c)
odd.ncols

val rankstream = new I_Rank_Stream(c)
val countstream = new I_Count_Stream(c)
val crankstream = new I_CRank_Stream(c)

KSP(50, 0.5).contrast(odd, Set(0, 1, 2))

MWP(50, 0.5).contrast(crankstream, Set(0, 1, 2))

CSP(50, 0.5).contrast(countstream, Set(0, 1, 2))

//time(countstream.insert(Array(1,2,3)))
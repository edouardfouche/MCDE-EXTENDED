import io.github.edouardfouche.index.dimension.DIS_Rank

val test1 = Array(2.0,6.0,1.0,3.0,4.0,5.0,7.0)
val odd1 = new DIS_Rank(test1)
odd1(0)
odd1(1)
odd1(2)
assert(odd1(0).toTuple == (2,1.0,0.0))
assert(odd1(1) == (0,2.0,0.0))
assert(odd1(2) == (3,3.0,0.0))
assert(odd1(3) == (4,4.0,0.0))
assert(odd1(4) == (5,5.0,0.0))
assert(odd1(5) == (1,6.0,0.0))
assert(odd1(6) == (6,7.0,0.0))

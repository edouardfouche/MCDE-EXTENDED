import io.github.edouardfouche.index.dimension.DIS_CRank

// init
print(s"Check init for odd")
val odd1 = new DIS_CRank(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
odd1

odd1.insert(1.0)
odd1

odd1.insert(1.0)
odd1

odd1.insert(6.0)
odd1

odd1.refresh
odd1

val odd2 = new DIS_CRank(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
odd2

odd2.insert(2.0)
odd2.insert(2.0)
odd2.insert(6.0)

odd2
odd2.refresh
odd2

odd2.insert(1.0)
odd2.insert(1.0)
odd2.insert(0.0)

odd2
odd2.refresh
odd2
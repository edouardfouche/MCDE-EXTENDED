import io.github.edouardfouche.index.dimension.D_Rank_Stream

// init
print(s"Check init for odd")
val odd1 = new D_Rank_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
odd1

odd1.insert(1.0)
odd1

odd1.insert(1.0)
odd1

odd1.insert(6.0)
odd1

odd1.refresh
odd1
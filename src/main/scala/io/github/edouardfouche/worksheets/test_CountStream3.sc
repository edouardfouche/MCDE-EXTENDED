import io.github.edouardfouche.index.dimension.DIS_Count

// init
print(s"Check init for odd")
val odd1 = new DIS_Count(Array(2.0, 2.0, 6.0, 1.0, 1, 0))
odd1(0)

odd1.insert(1.0)
odd1(0)

odd1.insert(1.0)
odd1(0)

odd1.insert(6.0)
odd1(0)

odd1.refresh
odd1(0)
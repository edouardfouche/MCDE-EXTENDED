import io.github.edouardfouche.index.DimensionIndex_CountStream

// init
print(s"Check init for odd")
val odd1 = new DimensionIndex_CountStream(Array(2.0,2.0,6.0,1.0,1.0,0.0))
odd1(0)

odd1.insert(1.0)
odd1(0)

odd1.insert(1.0)
odd1(0)

odd1.insert(6.0)
odd1(0)

odd1.refresh
odd1(0)
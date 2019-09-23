import io.github.edouardfouche.index.DimensionIndex_RankStream

// init
print(s"Check init for odd")
val odd1 = new DimensionIndex_RankStream(Array(2.0,6.0,1.0,3.0,4.0,5.0,7.0))
assert(odd1(0).toTuple == (2,1.0,0.0))
assert(odd1(1).toTuple == (0,2.0,0.0))
assert(odd1(2).toTuple == (3,3.0,0.0))
assert(odd1(3).toTuple == (4,4.0,0.0))
assert(odd1(4).toTuple == (5,5.0,0.0))
assert(odd1(5).toTuple == (1,6.0,0.0))
assert(odd1(6).toTuple == (6,7.0,0.0))

print(s"Check init for even")
val even1 = new DimensionIndex_RankStream(Array(4.0,2.0,6.0,1.0,3.0,5.0))
assert(even1(0).toTuple == (3,1.0,0.0))
assert(even1(1).toTuple == (1,2.0,0.0))
assert(even1(2).toTuple == (4,3.0,0.0))
assert(even1(3).toTuple == (0,4.0,0.0))
assert(even1(4).toTuple == (5,5.0,0.0))
assert(even1(5).toTuple == (2,6.0,0.0))

print(s"Check init for ties")
val ties1 = new DimensionIndex_RankStream(Array(3.0,2.0,2.0,1.0,2.0))
assert(ties1(0)._2 == 1.0)
assert(ties1(1)._2 == 2.0)
assert(ties1(2)._2 == 2.0)
assert(ties1(3)._2 == 2.0)
assert(ties1(4)._2 == 3.0)

// insert, odd
print(s"Check insert below delete for odd")
val odd = Array(2.0,6.0,1.0,3.0,4.0,5.0,7.0)
val odd2 = new DimensionIndex_RankStream(odd)
odd2.insert(1.5)
odd2
assert(odd2(1).toTuple == (7,1.5,0.0))

print(s"Check insert after delete for odd")
val odd3 = new DimensionIndex_RankStream(odd)
odd3.insert(2.5)
assert(odd3(1).toTuple == (7,2.5,0.0))

print(s"Check insert middle for odd")
val odd4 = new DimensionIndex_RankStream(odd)
odd4.insert(4.5)
assert(odd4(3).toTuple == (7,4.5,0.0))

print(s"Check insert begin for odd")
val odd5 = new DimensionIndex_RankStream(odd)
odd5.insert(0.5)
odd5
assert(odd5(0).toTuple == (7,0.5,0.0))

print(s"Check insert end for odd")
val odd6 = new DimensionIndex_RankStream(odd)
odd6.insert(7.5)
assert(odd6(6).toTuple == (7,7.5,0.0))

// insert, even
print(s"Check insert below delete for even")
val even = Array(4.0,2.0,6.0,1.0,3.0,5.0)
val even2 = new DimensionIndex_RankStream(even)
even2.insert(3.5)
assert(even2(3).toTuple == (6,3.5,0.0))

print(s"Check insert after delete for even")
val even3 = new DimensionIndex_RankStream(even)
even3.insert(4.5)
assert(even3(3).toTuple == (6,4.5,0.0))

print(s"Check insert middle for even")
val even4 = new DimensionIndex_RankStream(even)
even4.insert(2.5)
assert(even4(2).toTuple == (6,2.5,0.0))

print(s"Check insert begin for even")
val even5 = new DimensionIndex_RankStream(even)
even5.insert(0.5)
assert(even5(0).toTuple == (6,0.5,0.0))

print(s"Check insert end for even")
val even6 = new DimensionIndex_RankStream(even)
even6.insert(7.5)
assert(even6(5).toTuple == (6,7.5,0.0))

// insert, ties
print(s"Check insert below delete for ties")
val ties = Array(3.0,2.0,2.0,1.0,2.0)
val ties2 = new DimensionIndex_RankStream(ties)
ties2.insert(1.5)
ties2
assert(ties2(1).toTuple == (5,1.5,0.0))

print(s"Check insert after delete for ties")
val ties3 = new DimensionIndex_RankStream(ties)
ties3.insert(2.5)
assert(ties3(4).toTuple == (5,2.5,0.0))

print(s"Check insert middle for ties")
val ties4 = new DimensionIndex_RankStream(ties)
ties4.insert(2.0)
ties4
assert(ties4(1).toTuple == (1,2.0,0.0))
assert(ties4(2).toTuple == (2,2.0,0.0))
assert(ties4(3).toTuple == (4,2.0,0.0))
assert(ties4(4).toTuple == (5,2.0,0.0))

ties4.refresh

assert(ties4(0).toTuple == (2,1.0,0.0))
assert(ties4(1).toTuple == (0,2.0,0.0))
assert(ties4(2).toTuple == (1,2.0,0.0))
assert(ties4(3).toTuple == (3,2.0,0.0))
assert(ties4(4).toTuple == (4,2.0,0.0))

import io.github.edouardfouche.index.dimension.D_CRank_Stream

val odd1 = new D_CRank_Stream(Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0))

val even1 = new D_CRank_Stream(Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0))

val ties1 = new D_CRank_Stream(Array(3.0, 2.0, 2.0, 1.0, 2.0))

val odd = Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0)

val odd6 = new D_CRank_Stream(odd)
odd6.insert(7.5)
odd6

val even = Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0)

val even6 = new D_CRank_Stream(even)
even6.insert(7.5)
even6

val odd11 = new D_CRank_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
odd11.insert(1.0)
odd11
odd1.insert(1.0)
odd11
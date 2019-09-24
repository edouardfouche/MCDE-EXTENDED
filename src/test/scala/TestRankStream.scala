import io.github.edouardfouche.index.dimension.DIS_Rank
import org.scalatest.FunSuite

class TestRankStream extends FunSuite {

  val odd = Array(2.0,6.0,1.0,3.0,4.0,5.0,7.0)
  val even = Array(4.0,2.0,6.0,1.0,3.0,5.0)

  // init
  test(s"Check init for odd") {
    val odd1 = new DIS_Rank(Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0))
    assert(odd1(0).toTuple == (2, 1.0))
    assert(odd1(1).toTuple == (0, 2.0))
    assert(odd1(2).toTuple == (3, 3.0))
    assert(odd1(3).toTuple == (4, 4.0))
    assert(odd1(4).toTuple == (5, 5.0))
    assert(odd1(5).toTuple == (1, 6.0))
    assert(odd1(6).toTuple == (6, 7.0))
  }


  test(s"Check init for even") {
    val even1 = new DIS_Rank(Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0))
    assert(even1(0).toTuple == (3, 1.0))
    assert(even1(1).toTuple == (1, 2.0))
    assert(even1(2).toTuple == (4, 3.0))
    assert(even1(3).toTuple == (0, 4.0))
    assert(even1(4).toTuple == (5, 5.0))
    assert(even1(5).toTuple == (2, 6.0))
  }


  test(s"Check init for ties") {
    val ties1 = new DIS_Rank(Array(3.0, 2.0, 2.0, 1.0, 2.0))
    assert(ties1(0)._2 == 1.0)
    assert(ties1(1)._2 == 2.0)
    assert(ties1(2)._2 == 2.0)
    assert(ties1(3)._2 == 2.0)
    assert(ties1(4)._2 == 3.0)
  }


  // insert, odd
  test(s"Check insert below delete for odd") {
    val odd2 = new DIS_Rank(odd)
    odd2.insert(1.5)
    assert(odd2(1).toTuple == (7, 1.5))
  }


  test(s"Check insert after delete for odd") {
    val odd3 = new DIS_Rank(odd)
    odd3.insert(2.5)
    assert(odd3(1).toTuple == (7, 2.5))
  }

  test(s"Check insert middle for odd") {
    val odd4 = new DIS_Rank(odd)
    odd4.insert(4.5)
    assert(odd4(3).toTuple == (7, 4.5))
  }


  test(s"Check insert begin for odd") {
    val odd5 = new DIS_Rank(odd)
    odd5.insert(0.5)
    assert(odd5(0).toTuple == (7, 0.5))
  }

  test(s"Check insert end for odd") {
    val odd6 = new DIS_Rank(odd)
    odd6.insert(7.5)
    assert(odd6(6).toTuple == (7, 7.5))
  }


  // insert, even
  test(s"Check insert below delete for even") {

    val even2 = new DIS_Rank(even)
    even2.insert(3.5)
    assert(even2(3).toTuple == (6, 3.5))
  }

  test(s"Check insert after delete for even") {
    val even3 = new DIS_Rank(even)
    even3.insert(4.5)
    assert(even3(3).toTuple == (6, 4.5))
  }

  test(s"Check insert middle for even") {
    val even4 = new DIS_Rank(even)
    even4.insert(2.5)
    assert(even4(2).toTuple == (6, 2.5))
  }

  test(s"Check insert begin for even") {
    val even5 = new DIS_Rank(even)
    even5.insert(0.5)
    assert(even5(0).toTuple == (6, 0.5))

  }

  test(s"Check insert end for even") {
    val even6 = new DIS_Rank(even)
    even6.insert(7.5)
    assert(even6(5).toTuple == (6, 7.5))
  }

  // insert, ties
  val tiesA = Array(3.0,2.0,2.0,1.0,2.0)
  test(s"A - Check insert below delete for ties") {
    val ties2 = new DIS_Rank(tiesA)
    ties2.insert(1.5)
    assert(ties2(1).toTuple == (5, 1.5))
  }

  test(s"A - Check insert after delete for ties") {
    val ties3 = new DIS_Rank(tiesA)
    ties3.insert(2.5)
    assert(ties3(4).toTuple == (5, 2.5))
  }

  test(s"A - Check insert middle for ties") {
    val ties4 = new DIS_Rank(tiesA)
    ties4.insert(2.0)
    assert(ties4(1).toTuple == (1, 2.0))
    assert(ties4(2).toTuple == (2, 2.0))
    assert(ties4(3).toTuple == (4, 2.0))
    assert(ties4(4).toTuple == (5, 2.0))
    ties4.refresh
    assert(ties4(0).toTuple == (2, 1.0))
    assert(ties4(1).toTuple == (0, 2.0))
    assert(ties4(2).toTuple == (1, 2.0))
    assert(ties4(3).toTuple == (3, 2.0))
    assert(ties4(4).toTuple == (4, 2.0))
  }

  val tiesB = Array(3.0,2.0,2.0,4.0,2.0)
  test(s"B - Check insert below delete for ties") {
    val ties5 = new DIS_Rank(tiesB)
    ties5.insert(1.5)
    assert(ties5(0).toTuple == (5, 1.5))
  }

  test(s"B - Check insert after delete for ties") {
    val ties6 = new DIS_Rank(tiesB)
    ties6.insert(2.5)
    assert(ties6(3).toTuple == (5, 2.5))
  }


  test(s"B - Check insert middle for ties") {
    val ties7 = new DIS_Rank(tiesB)
    ties7.insert(2.0)
    assert(ties7(0).toTuple == (1, 2.0))
    assert(ties7(1).toTuple == (2, 2.0))
    assert(ties7(2).toTuple == (4, 2.0))
    assert(ties7(3).toTuple == (5, 2.0))
  }

  val tiesC = Array(0.0,2.0,2.0,1.0,2.0)
  test(s"C - Check insert below delete for ties") {

    val ties8 = new DIS_Rank(tiesC)
    ties8.insert(1.5)
    assert(ties8(1).toTuple == (5, 1.5))
  }


  test(s"C - Check insert after delete for ties") {
    val ties9 = new DIS_Rank(tiesC)
    ties9.insert(2.5)
    assert(ties9(4).toTuple == (5, 2.5))
  }


  test(s"C - Check insert middle for ties") {
    val ties10 = new DIS_Rank(tiesC)
    ties10.insert(2.0)
    assert(ties10(1).toTuple == (1, 2.0))
    assert(ties10(2).toTuple == (2, 2.0))
    assert(ties10(3).toTuple == (4, 2.0))
    assert(ties10(4).toTuple == (5, 2.0))
  }

  val tiesD = Array(2.0,0.0,2.0,1.0,2.0)
  test(s"D - Check insert below delete for ties") {

    val ties8 = new DIS_Rank(tiesD)
    ties8.insert(1.5)
    assert(ties8(2).toTuple == (5, 1.5))
  }


  test(s"D - Check insert after delete for ties") {
    val ties9 = new DIS_Rank(tiesD)
    ties9.insert(2.5)
    assert(ties9(4).toTuple == (5, 2.5))
  }


  test(s"D - Check insert middle for ties") {
    val ties10 = new DIS_Rank(tiesD)
    //println(ties10)
    ties10.insert(2.0)
    //println(ties10)
    assert(ties10(1).toTuple == (3, 1.0))
    assert(ties10(2).toTuple == (2, 2.0))
    assert(ties10(3).toTuple == (4, 2.0))
    assert(ties10(4).toTuple == (5, 2.0))
  }

  val tiesE = Array(2.0,3.0,2.0,1.0,2.0)
  test(s"E - Check insert below delete for ties") {
    val ties8 = new DIS_Rank(tiesE)
    ties8.insert(1.5)
    assert(ties8(1).toTuple == (5, 1.5))
  }


  test(s"E - Check insert after delete for ties") {
    val ties9 = new DIS_Rank(tiesE)
    ties9.insert(2.5)
    assert(ties9(3).toTuple == (5, 2.5))
  }


  test(s"E - Check insert middle for ties") {
    val ties10 = new DIS_Rank(tiesE)
    //println(ties10)
    ties10.insert(2.0)
    //println(ties10)
    assert(ties10(1).toTuple == (2, 2.0))
    assert(ties10(2).toTuple == (4, 2.0))
    assert(ties10(3).toTuple == (5, 2.0))
    assert(ties10(4).toTuple == (1, 3.0))
  }

  val tiesF = Array(2.0,3.0,2.0,4.0,2.0)
  test(s"F - Check insert below delete for ties") {
    val ties8 = new DIS_Rank(tiesF)
    ties8.insert(1.5)
    assert(ties8(0).toTuple == (5, 1.5))
  }


  test(s"F - Check insert after delete for ties") {
    val ties9 = new DIS_Rank(tiesF)
    ties9.insert(2.5)
    assert(ties9(2).toTuple == (5, 2.5))
  }


  test(s"F - Check insert middle for ties") {
    val ties10 = new DIS_Rank(tiesF)
    ties10.insert(2.0)
    assert(ties10(0).toTuple == (2, 2.0))
    assert(ties10(1).toTuple == (4, 2.0))
    assert(ties10(2).toTuple == (5, 2.0))
    assert(ties10(3).toTuple == (1, 3.0))
  }

  test(s"Test Scenario 1") {
    val odd1 = new DIS_Rank(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd1.insert(1.0)
    assert(odd1(0).toTuple == (5, 0.0))
    assert(odd1(1).toTuple == (3, 1.0))
    assert(odd1(2).toTuple == (4, 1.0))
    assert(odd1(3).toTuple == (6, 1.0))
    assert(odd1(4).toTuple == (1, 2.0))

    odd1.insert(1.0)
    assert(odd1(0).toTuple == (5, 0.0))
    assert(odd1(1).toTuple == (3, 1.0))
    assert(odd1(2).toTuple == (4, 1.0))
    assert(odd1(3).toTuple == (6, 1.0))
    assert(odd1(4).toTuple == (7, 1.0))
    assert(odd1(5).toTuple == (2, 6.0))

    odd1.insert(6.0)
    assert(odd1(0).toTuple == (5, 0.0))
    assert(odd1(1).toTuple == (3, 1.0))
    assert(odd1(2).toTuple == (4, 1.0))
    assert(odd1(3).toTuple == (6, 1.0))
    assert(odd1(4).toTuple == (7, 1.0))
    assert(odd1(5).toTuple == (8, 6.0))

    odd1.refresh
    assert(odd1(0).toTuple == (2, 0.0))
    assert(odd1(1).toTuple == (0, 1.0))
    assert(odd1(2).toTuple == (1, 1.0))
    assert(odd1(3).toTuple == (3, 1.0))
    assert(odd1(4).toTuple == (4, 1.0))
    assert(odd1(5).toTuple == (5, 6.0))
  }

  test("Test Scenario 2") {
    val odd2 = new DIS_Rank(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd2.insert(2.0)
    odd2.insert(2.0)
    odd2.insert(6.0)
    assert(odd2(0).toTuple == (5, 0.0))
    assert(odd2(1).toTuple == (3, 1.0))
    assert(odd2(2).toTuple == (4, 1.0))
    assert(odd2(3).toTuple == (6, 2.0))
    assert(odd2(4).toTuple == (7, 2.0))
    assert(odd2(5).toTuple == (8, 6.0))

    odd2.refresh
    assert(odd2(0).toTuple == (2, 0.0))
    assert(odd2(1).toTuple == (0, 1.0))
    assert(odd2(2).toTuple == (1, 1.0))
    assert(odd2(3).toTuple == (3, 2.0))
    assert(odd2(4).toTuple == (4, 2.0))
    assert(odd2(5).toTuple == (5, 6.0))

    odd2.insert(1.0)
    odd2.insert(1.0)
    odd2.insert(0.0)

    assert(odd2(0).toTuple == (8, 0.0))
    assert(odd2(1).toTuple == (6, 1.0))
    assert(odd2(2).toTuple == (7, 1.0))
    assert(odd2(3).toTuple == (3, 2.0))
    assert(odd2(4).toTuple == (4, 2.0))
    assert(odd2(5).toTuple == (5, 6.0))

    odd2.insert(10.0)
    odd2.insert(-1.0)
    assert(odd2(0).toTuple == (10, -1.0))
    assert(odd2(5).toTuple == (9, 10.0))

    odd2.refresh

    assert(odd2(0).toTuple == (5, -1.0))
    assert(odd2(1).toTuple == (3, 0.0))
    assert(odd2(2).toTuple == (1, 1.0))
    assert(odd2(3).toTuple == (2, 1.0))
    assert(odd2(4).toTuple == (0, 6.0))
    assert(odd2(5).toTuple == (4, 10.0))
  }


}

/*
 * Copyright (C) 2018 Edouard Fouché
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

import io.github.edouardfouche.mcde.MWP
import org.scalatest.FunSuite

class TestPreprocess extends FunSuite with TestData {
  test("MW Preprocessing works as expected, test case 1: Duplicates 'in the middle'") {
    val a = scala.util.Random.shuffle(Array(0, 1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9).toList).toArray
    val c = MWP(50).preprocess(Array(a.map(_.toDouble)).transpose)
    assert(c(0)(1).rank === 1.5)
    assert(c(0)(2).rank === 1.5)
    assert(c(0)(6).rank === 7.0)
    assert(c(0)(7).rank === 7.0)
    assert(c(0)(8).rank === 7.0)
  }

  test("MW Preprocessing works as expected, test case 2. Duplicates at start and end") {
    val a = scala.util.Random.shuffle(Array(0, 0, 0, 2, 3, 4, 5, 5, 5, 6, 9, 9, 9).toList).toArray
    val c = MWP(50).preprocess(Array(a.map(_.toDouble)).transpose)
    assert(c(0)(0).rank === 1.0)
    assert(c(0)(1).rank === 1.0)
    assert(c(0)(2).rank === 1.0)
    assert(c(0)(10).rank === 11.0)
    assert(c(0)(11).rank === 11.0)
    assert(c(0)(12).rank === 11.0)
  }

  test("MW Preprocessing works as expected, test case 3: No Duplicates") {
    val a = scala.util.Random.shuffle(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).toList).toArray
    val c = MWP(50).preprocess(Array(a.map(_.toDouble)).transpose)
    assert(c(0)(0).rank === 0.0)
    assert(c(0)(1).rank === 1.0)
    assert(c(0)(2).rank === 2.0)
    assert(c(0)(3).rank === 3.0)
    assert(c(0)(4).rank === 4.0)
    assert(c(0)(5).rank === 5.0)
    assert(c(0)(6).rank === 6.0)
    assert(c(0)(7).rank === 7.0)
    assert(c(0)(8).rank === 8.0)
    assert(c(0)(9).rank === 9.0)
  }
}

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
import io.github.edouardfouche.preprocess.DataSet
import org.scalatest.FunSuite

class TestPreprocess extends FunSuite with TestData {
  test("MW Preprocessing works as expected, test case 1: Duplicates 'in the middle'") {
    val a = scala.util.Random.shuffle(Array(0, 1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9).toList).toArray
    val c = MWP(50).preprocess(new DataSet(Array(a.map(_.toDouble))))
    assert(c(0)(1).value === 1.5)
    assert(c(0)(2).value === 1.5)
    assert(c(0)(6).value === 7.0)
    assert(c(0)(7).value === 7.0)
    assert(c(0)(8).value === 7.0)
  }

  test("MW Preprocessing works as expected, test case 2. Duplicates at start and end") {
    val a = scala.util.Random.shuffle(Array(0, 0, 0, 2, 3, 4, 5, 5, 5, 6, 9, 9, 9).toList).toArray
    val c = MWP(50).preprocess(new DataSet(Array(a.map(_.toDouble))))
    assert(c(0)(0).value === 1.0)
    assert(c(0)(1).value === 1.0)
    assert(c(0)(2).value === 1.0)
    assert(c(0)(10).value === 11.0)
    assert(c(0)(11).value === 11.0)
    assert(c(0)(12).value === 11.0)
  }

  test("MW Preprocessing works as expected, test case 3: No Duplicates") {
    val a = scala.util.Random.shuffle(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).toList).toArray
    val c = MWP(50).preprocess(new DataSet(Array(a.map(_.toDouble))))
    assert(c(0)(0).value === 0.0)
    assert(c(0)(1).value === 1.0)
    assert(c(0)(2).value === 2.0)
    assert(c(0)(3).value === 3.0)
    assert(c(0)(4).value === 4.0)
    assert(c(0)(5).value === 5.0)
    assert(c(0)(6).value === 6.0)
    assert(c(0)(7).value === 7.0)
    assert(c(0)(8).value === 8.0)
    assert(c(0)(9).value === 9.0)
  }
}

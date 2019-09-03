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
package io.github.edouardfouche.index

import io.github.edouardfouche.index.tuple.TupleIndex
import io.github.edouardfouche.preprocess.Preprocess

//TODO: Refactor the Slice1, Slice2, Slice3
/**
  * Compute an adjusted rank index from a given data set
  * The "adjusted rank" means that in the case of ties, the rank is defined as the average rank of the tying values
  *
  * @param values A row-oriented data set
  */
class DimensionIndex_AdjustedRank(val values: Array[Double]) extends DimensionIndex[Double]  {
   def createIndex(input: Array[Double]): Array[_ <: TupleIndex] = {
    Process.mwRank(input)
  }
}

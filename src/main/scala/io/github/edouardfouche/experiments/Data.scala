/*
 * Copyright (C) 2020 Edouard Fouché
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
package io.github.edouardfouche.experiments

import io.github.edouardfouche.preprocess.DataRef

object Data {
  // possibly replace with your own data set
  lazy val bioliq = DataRef("bioliq", home + "/data/bioliq/bioliq_1w_interesting.csv", 1, ",", "bioliq")
  val currentdir: String = System.getProperty("user.dir")
  val home: String = System.getProperty("user.home")
}

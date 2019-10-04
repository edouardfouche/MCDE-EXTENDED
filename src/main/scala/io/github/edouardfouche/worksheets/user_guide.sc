/**
  * ###### Using MCDE in your code ######
  */


/**
  * ### Importing ###
  *
  * Import all MCDE classes or just the one you need.
  */

import io.github.edouardfouche.mcde._
// import io.github.edouardfouche.mcde.{MWP, MWPi, MWPr, MWPs, MWPu, KSPs}

// Optionally import data generators for creating given dependencies.
// For more information see: https://github.com/edouardfouche/DataGenerator
import io.github.edouardfouche.generators.Independent

// For loading data from a csv file
import io.github.edouardfouche.preprocess.Preprocess

/**
  * ### Data Generation & loading from CSV ###
  *
  * Note that MCDE classes expect an Array[Array[Double] which
  * is tuple oriented (Array containing Arrays which contain each row/tuple). Therefore we transpose the data.
  */

// Generating linear bivariate data.
val attribute1: Array[Double] = (1 to 100).map(_.toDouble).toArray
val attribute2: Array[Double] = attribute1.map(x => x * 2)
val linear_2: Array[Array[Double]] = Array(attribute1, attribute2).transpose // Dim: 100 x 2

// Generating linear multivariate data.
val attribute3: Array[Double] = attribute1.map(_ * 4)
val attribute4: Array[Double] = attribute1.map(_ * 6)
val linear_4: Array[Array[Double]] = Array(attribute1, attribute2, attribute3, attribute4).transpose

// Generating independent data using the data generator class (https://github.com/edouardfouche/DataGenerator)
val independent_generator = Independent(5, 0.0, "gaussian", 0)
val independent = independent_generator.generate(100000) // Dim: 100000 x 5

/**
  * Loading data from csv. Each row in the file must be tuple, each col must be attribute
  * @header Number of lines to discard (header), by default 1.
  * @excludeIndex Whether to exclude an index (the first column) or not.
  * @dropClass Keep always at true
  */

val path = s"${System.getProperty("user.dir")}/"
independent_generator.save(100, path)
val loaded_data = Preprocess.open(path + independent_generator.id + ".csv", header = 1, separator = ",", excludeIndex = false, dropClass = true)


/**
  * ### Create an instance of MWP class ####
  *
  * Note that MWP() is a case class.
  *
  * @M:Int              Number of repetitions. Default = 50
  * @alpha:Double       Expected share of instances in slice (independent dimensions). Default = 0.5
  * @beta:Double        Expected share of instances in marginal restriction (reference dimension). Default = 0.5
  *                     Added with respect to the original paper to loose the dependence of beta from alpha.
  * @parallelize:Int    Level of parallelization. 0: Single Core, 1: No. of cores set automatically,
  *                     >1: Specific no. of cores. Default = 0
  */

val mwp = MWP(M = 50, alpha = 0.5, beta = 0.5, parallelize = 0)

/**
  * ### Computing contrast scores ###
  *
  * Calling the contrast() method computes the dependency score including all specified dimensions. Scores may range
  * between 0.0 and 1.0. Strong dependecies usually have scores close to 1.0. Independent data usually yields scores around
  * 0.5. There are special cases where scores << 0.5 may arise (which also can be interpreted as independence)
  *
  * @m:Array[Array[Double]]   data (row oriented)
  * @dimensions:Set[Int]      Dimensions of the subspace on which the dependency should be estimated starting from 0
  */

val score:Double = mwp.contrast(m = linear_2, dimensions = Set(0,1)) // Note that MWP should generate values close to 1.0
println(mwp.contrast(linear_4, Set(0,1,2,3))) // Note that MWP should generate values close to 1.0
println(mwp.contrast(independent, Set(0,1,2,3,4))) // Note that MWP should generate values around to 0.5

println(mwp.contrast(linear_4, Set(0,3))) // Include only a subspace of your dimensions e.g. only attribute 1 and 4
println(mwp.contrast(linear_4, linear_4(0).indices.toSet)) // Include all dimensions without explicitly specifying

/**
  * Calling the contrastMatrix() computes the dependencie matrix including all dimensions (one to one dependencies)
  *
  * @m:Array[Array[Double]] data (row oriented)
  */

val scoreMatrix:Array[Array[Double]] = mwp.contrastMatrix(m = linear_4)
println(mwp.contrastMatrix(independent))


/**
  * ### Variations of MWP ###
  * Everything works equivalently for the other variations of MWP
  */

// MWPi: Like MWP but not adjusting for ties (but still adjusting for ranks)
// val mwpi = MWPi()

// MWPr: Like MWP but not adjusting and not correcting for ties (see Paper, Algorithm description)
val mwpr = MWPr()

// MWPs: Like MWP but also adjusting for ties in the slicing process
// val mwps = MWPs()

// MWPu: Like MWP but without border effects
// val mwpu = MWPu()

/**
  * KSPs: Like MWP but using Kolmogorow-Smirnow-Test for dependency estimation instead of Mann–Whitney P test
  *
  * Note that beta default value for KSPs is 1.0. It is not recommended to change this value.
  * All other parameters are defined as for MWP.
  *
  */

val ksp = KSPs(alpha = 0.1, beta = 1.0)
println(ksp.contrast(linear_4, linear_4(0).indices.toSet))
println(ksp.contrast(independent, independent(0).indices.toSet))
import java.io.File
import java.nio.file.{Files, Paths}

import io.github.edouardfouche.generators._
import io.github.edouardfouche.index._
import io.github.edouardfouche.index.dimension.DimensionIndex
import io.github.edouardfouche.mcde.{Stats, _}
import io.github.edouardfouche.preprocess.{DataSet, _}
import org.apache.commons.io.FileUtils
import org.scalatest.FunSuite

import scala.annotation.tailrec
import scala.language.existentials // Fixes a curious warning.



class TestDimensions extends FunSuite {

  val rows = 50
  val dims = 4
  val arr: Array[Array[Double]] = Independent(dims, 0.0, "gaussian", 0).generate(rows).transpose
  val bivar_arr: Array[Array[Double]] = Independent(2, 0.0, "gaussian", 0).generate(rows).transpose

  // TODO: What if new Tests / Generators?
  val all_mcde_stats:List[McdeStats] = List(KSP(), MWP())


  val all_indices = Array(new I_CRank(new DataSet(arr))) //, new DI_CRank(arr),
    //new DimensionIndex_Dummy(arr), new DImensionIndex_Rank(arr))

  val all_bivar_indices = Array(new I_CRank(new DataSet(bivar_arr))) //, new DI_CRank(bivar_arr),
    //new DimensionIndex_Dummy(bivar_arr), new DImensionIndex_Rank(bivar_arr))

  val all_gens = List(
    Cross(dims, 0.0,"gaussian", 0).generate(rows),
    Cubic(dims, 0.0,"gaussian", 0)().generate(rows),
    DoubleLinear(dims, 0.0,"gaussian", 0)().generate(rows),
    Hourglass(dims, 0.0,"gaussian", 0).generate(rows),
    Hypercube(dims, 0.0,"gaussian", 0).generate(rows),
    HypercubeGraph(dims, 0.0,"gaussian", 0).generate(rows),
    HyperSphere(dims, 0.0,"gaussian", 0).generate(rows),
    Independent(dims, 0.0,"gaussian", 0).generate(rows),
    Linear(dims, 0.0,"gaussian", 0).generate(rows),
    LinearPeriodic(dims, 0.0,"gaussian", 0)().generate(rows),
    LinearStairs(dims, 0.0,"gaussian", 0)().generate(rows),
    LinearThenDummy(dims, 0.0,"gaussian", 0).generate(rows),
    LinearThenNoise(dims, 0.0,"gaussian", 0).generate(rows),
    NonCoexistence(dims, 0.0,"gaussian", 0).generate(rows),
    RandomSteps(4, dims,"gaussian", 0)().generate(rows),
    Sine(dims, 0.0,"gaussian", 0)().generate(rows),
    Star(dims, 0.0,"gaussian", 0).generate(rows),
    StraightLines(dims, 0.0,"gaussian", 0).generate(rows),
    Z(dims, 0.0,"gaussian", 0).generate(rows),
    Zinv(dims, 0.0,"gaussian", 0).generate(rows))



  val path = s"${System.getProperty("user.home")}/datagenerator_for_scalatest/"
  val dir: Boolean = new File(path).mkdirs()
  val indi = Independent(dims, 0.0,"gaussian", 0)
  indi.save(rows,path) // save is final on Base Class, dir gets destructed after test
  val data: DataSet = Preprocess.open(path + indi.id + ".csv", header = 1, separator = ",", excludeIndex = false, dropClass = true)
  val dataclass = DataRef("Independent-2-0.0", path + indi.id + ".csv", 1, ",", "Test")



  def get_dim[T](arr: Vector[DimensionIndex]): (Int, Int) = {
    (arr.length, arr(0).length)
  }

  def which_row_orient_stats(stats: List[McdeStats]): List[Boolean] = {
      {for{
        stat <- stats
        data = stat.preprocess(new DataSet(arr))
      } yield get_dim(data.index)}.map(x => x == (dims, rows))
  }

  def which_row_orient_bivar_stats(stats: List[Stats]): List[Boolean] = {
    {for{
      stat <- stats
      data = stat.preprocess(new DataSet(bivar_arr))
    } yield get_dim(data.index)}.map(x => x == (2, rows))
  }

  def which_row_orient_index(ind: Array[_ <: Index[DimensionIndex]]):Array[Boolean] = {
      {for {
        index <- ind
      } yield get_dim(index.index)}.map(x => x == (dims, rows))

  }

  test("Checking if generated data is row oriented"){

    @tailrec
    def test_dim (arr_l: List[Array[Array[Double]]], size: Int = 0, tru: Int = 0): Boolean = {
      if(arr_l == Nil) size == tru
      else if ((arr_l.head.length, arr_l.head(0).length) == (rows, dims)) test_dim(arr_l.tail, size + 1 , tru + 1)
      else test_dim(arr_l.tail, size +1, tru)
    }

    assert(test_dim(all_gens))
  }

  test("Checking if val index is col oriented for all Stats"){
    which_row_orient_stats(all_mcde_stats).map(x => assert(x))
  }

  // To be sure we may be testing twice the same stuff

  test("Checking if val index is col oriented for all Indexstructures"){
    which_row_orient_index(all_indices).map(x => assert(x))
  }

  test("Checking if no of rows in saved data by saveSample != dims"){
    assert(data.nrows != dims)
  }

  test("Checking if saved data using DataGenerator.saveSample loads with right number of cols " +
    "using Preprocess.open() and DataRef(...).open()"){
    val dataclassData = dataclass.open()
    assert(data.ncols == dims)
    assert(dataclassData.ncols == dims)
  }

  test("Checking if DataRef(...).openAndPreprocess() loads col oriented data"){

    for{
      stat <- all_mcde_stats
      data = dataclass.openAndPreprocess(stat).index
    } assert(get_dim(data)._1 == dims)
  }

  test("Dir Destructor"){
    FileUtils.deleteDirectory(new File(path))
    assert(Files.notExists(Paths.get(path)))
  }


}

package io.github.edouardfouche.experiments

import java.io.{File, FileWriter}

import com.typesafe.scalalogging.LazyLogging
import io.github.edouardfouche.generators.{DataGenerator, _}
import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.DataSet
import io.github.edouardfouche.utils.StopWatch
import org.slf4j.MDC

/**
  * Created by fouchee on 26.07.17.
  */
trait Experiment extends LazyLogging {

  // output formatting
  val output_folder: String = System.getProperty("user.dir")
  val master_experiment_folder: String = output_folder concat "/" concat "experiments"
  utils.createFolderIfNotExisting(master_experiment_folder)
  val formatter = new java.text.SimpleDateFormat("yyy-MM-dd-HH-mm")
  val dirname: String = s"${formatter.format(java.util.Calendar.getInstance().getTime)}_${this.getClass.getSimpleName.init}_"
  val experiment_folder: String = master_experiment_folder concat "/" concat dirname
  val summaryPath = experiment_folder + "/" + this.getClass.getSimpleName.init + ".csv"

  MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")

  info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting the experiment ${this.getClass.getSimpleName.init}\n")
  utils.createFolderIfNotExisting(experiment_folder)

  info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

  def run(): Unit

  def info(s: String): Unit = {
    // Repeat the MDC so that we are sure that, even if we are in a subprocess, that the information will be logged centrally
    MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")
    logger.info(s)
  }

  val defaulttests: Vector[McdeStats] = Vector(MWP(50,alpha=0.5,beta=0.5),
    KSPs(50, alpha = 0.5, beta = 0.5),
    CSP(50,alpha=0.5,beta=0.5),
    MCDEP(50,alpha=0.5,beta=0.5))

  val selected_generators: Vector[(Int, Double, String, Int) => DataGenerator] = Vector(
    Cross,
    DoubleLinear(_:Int,_:Double,_:String,_:Int)(Some(0.25)),
    Hourglass,
    Hypercube,
    HypercubeGraph,
    HyperSphere,
    Linear,
    Parabola(_:Int,_:Double,_:String,_:Int)(scale=Some(1)),
    Sine(_:Int,_:Double,_:String,_:Int)(period = Some(1)),
    Sine(_:Int,_:Double,_:String,_:Int)(period = Some(5)),
    Star,
    Zinv,
    Independent
  )

  val all_generators: Vector[(Int, Double, String, Int) => DataGenerator] = Vector(
    Cross,
    DoubleLinear(_:Int,_:Double,_:String,_:Int)(coef=Some(0.25)),
    DoubleLinear(_:Int,_:Double,_:String,_:Int)(coef=Some(0.5)),
    DoubleLinear(_:Int,_:Double,_:String,_:Int)(coef=Some(0.75)),
    Parabola(_:Int,_:Double,_:String,_:Int)(scale=Some(1)),
    Parabola(_:Int,_:Double,_:String,_:Int)(scale=Some(2)),
    Parabola(_:Int,_:Double,_:String,_:Int)(scale=Some(3)),
    Hourglass, Hypercube, HypercubeGraph,
    Independent,
    Linear,
    LinearPeriodic(_:Int,_:Double,_:String,_:Int)(period = Some(2)),
    LinearPeriodic(_:Int,_:Double,_:String,_:Int)(period = Some(5)),
    LinearPeriodic(_:Int,_:Double,_:String,_:Int)(period = Some(10)),
    LinearPeriodic(_:Int,_:Double,_:String,_:Int)(period = Some(20)),
    LinearStairs(_:Int,_:Double,_:String,_:Int)(Some(2)),
    LinearStairs(_:Int,_:Double,_:String,_:Int)(Some(5)),
    LinearStairs(_:Int,_:Double,_:String,_:Int)(Some(10)),
    LinearStairs(_:Int,_:Double,_:String,_:Int)(Some(20)),
    LinearSteps(_:Int,_:Double,_:String,_:Int)(Some(2)),
    LinearSteps(_:Int,_:Double,_:String,_:Int)(Some(5)),
    LinearSteps(_:Int,_:Double,_:String,_:Int)(Some(10)),
    LinearSteps(_:Int,_:Double,_:String,_:Int)(Some(20)),
    LinearThenDummy,
    LinearThenNoise,
    NonCoexistence,
    Cubic(_:Int,_:Double,_:String,_:Int)(Some(1)),Cubic(_:Int,_:Double,_:String,_:Int)(Some(2)),Cubic(_:Int,_:Double,_:String,_:Int)(Some(3)),
    RandomSteps(_:Int,_:Double,_:String,_:Int)(Some(2)), RandomSteps(_:Int,_:Double,_:String,_:Int)(Some(5)),
    RandomSteps(_:Int,_:Double,_:String,_:Int)(Some(10)), RandomSteps(_:Int,_:Double,_:String,_:Int)(Some(20)),
    Sine(_:Int,_:Double,_:String,_:Int)(Some(2)), Sine(_:Int,_:Double,_:String,_:Int)(Some(5)),
    Sine(_:Int,_:Double,_:String,_:Int)(Some(10)), Sine(_:Int,_:Double,_:String,_:Int)(Some(20)),
    HyperSphere,
    Root(_:Int,_:Double,_:String,_:Int)(Some(1)), Root(_:Int,_:Double,_:String,_:Int)(Some(2)),
    Root(_:Int,_:Double,_:String,_:Int)(Some(3)),
    Star,
    StraightLines,
    Z,
    Zinv
  )



  def compareContrast(generators: Vector[DataGenerator], tests: Vector[Stats], rep: Int): Unit = {
    for {
      generator <- generators.par
    } {
      val raw = new DataSet(generator.generate(1000).transpose, types=(1 to generator.nDim).toArray.map(x => "c"))

      // Save data samples (debugging purpose)
      utils.createFolderIfNotExisting(experiment_folder + "/data")
      if (rep == 1) utils.saveDataSet(raw.columns.transpose, experiment_folder + "/data/" + s"${generator.id}")
      for {
        test <- tests
      } {
        val (prepCPUtime, prepWalltime, data) = StopWatch.measureTime(test.preprocess(raw))
        val (runCPUtime, runWalltime, contrast) = StopWatch.measureTime(test.contrast(data, data.indices.toSet))

        val attributes = List("refId", "testId", "ncols", "nrows", "alpha", "beta", "M", "prepCPUtime", "prepWalltime",
        "runCPUtime", "runWalltime", "contrast", "rep")
        val summary = ExperimentSummary(attributes)
        summary.add("refId", generator.id)
        summary.add("testId", test.id)
        summary.add("ncols", data.ncols)
        summary.add("nrows", data.nrows)
        summary.add("alpha", test.alpha)
        summary.add("beta", test.beta)
        summary.add("M", test.M)

        summary.add("prepCPUtime", prepCPUtime)
        summary.add("prepWalltime", prepWalltime)

        summary.add("runCPUtime", runCPUtime)
        summary.add("runWalltime", runWalltime)

        summary.add("contrast", contrast)
        summary.add("rep", rep)

        summary.write(summaryPath)
      }
    }
  }

  case class ExperimentSummary(attributes: List[String]) {
    //var results: List[(String, Any)] = List()
    val results: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    //def add(name: String, v: Any): Unit = results = results :+ (name, v)
    def add(name: String, v: Any): Unit = {
      results(name) = v
    }

    def write(path: String): Unit = {
      synchronized {
        if(!new File(path).exists) { // write the header
          val fileA = new File(path)
          val fwA = new FileWriter(fileA, true)
          fwA.write(getHeader)
          fwA.flush()
          fwA.close()
        }
        val fileA = new File(path)
        val fwA = new FileWriter(fileA, true) // append set to true
        fwA.write(this.toString) // this is the string
        fwA.flush()
        fwA.close()
      }
    }

    override def toString: String = {
      (attributes.map(x => results.getOrElse(x, "NULL").toString) mkString ",") + "\n"
    }

    def getHeader: String = (attributes mkString ",") + "\n"
  }
}

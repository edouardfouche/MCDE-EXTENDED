package io.github.edouardfouche.experiments

import io.github.edouardfouche.preprocess.DataRef


object Data {
  lazy val bioliq20 = DataRef("bioliq", home + "/data/bioliq/bioliq-I-07-2016/bioliq_1wx20.csv", 1, ",", "bioliq")
  lazy val bioliq_full = DataRef("bioliq", home + "/data/bioliq/bioliq_1w_full.csv", 1, ",", "bioliq")
  lazy val bioliq_interesting = DataRef("bioliq", home + "/data/bioliq/bioliq_1w_interesting.csv", 1, ",", "bioliq")
  val currentdir: String = System.getProperty("user.dir")
  val home: String = System.getProperty("user.home")

  // helper function to get the relative path to resources
  def fetch(path: String): String = try {
    val fullpath = currentdir + "/data" + path
    val s = scala.io.Source.fromFile(fullpath)
    s.close()
    fullpath
  } catch {
    case _: Throwable => {
      try {
        val fullpath2 = currentdir + "/git/docLOF/data" + path
        val s = scala.io.Source.fromFile(fullpath2)
        s.close()
        fullpath2
      } catch {
        case _: Throwable => {
          val message = s"Tried ${currentdir + "/data" + path} and ${currentdir + "/git/docLOF/data" + path}"
          throw new Error(message)
        }
      }
    }
  }
}

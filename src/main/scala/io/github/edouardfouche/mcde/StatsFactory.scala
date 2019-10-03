package io.github.edouardfouche.mcde

object StatsFactory {
  def getTest(test: String, m: Int, alpha: Double, beta: Double, parallelize: Int): Stats =
  test.toLowerCase match {
    case "autop" => MCDEP(m, alpha, beta, parallelize)
    case "csp" => CSP(m, alpha, beta, parallelize)
    case "ksp" => KSP_bis(m, alpha, beta, parallelize)
    case "kspn" => KSP_bisn(m, alpha, beta, parallelize)
    case "kspp" => KSP(m, alpha, beta, parallelize)
    case "mwp" => MWP(m, alpha, beta, parallelize)
    case "mwpr" => MWPr(m, alpha, beta, parallelize)
    //case "mwpi" => MWPi(m, alpha, beta, parallelize)
    //case "mwpr" => MWPr(m, alpha, beta, parallelize)
    //case "mwps" => MWPs(m, alpha, beta, parallelize)
    //case "mwpu" => MWPu(m, alpha, beta, parallelize)
    case _ => throw new Error(s"Unknown statistical test $test")
  }
}

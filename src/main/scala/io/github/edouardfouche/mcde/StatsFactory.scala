package io.github.edouardfouche.mcde

object StatsFactory {
  def getTest(test: String, m: Int, alpha: Double, beta: Double, parallelize: Int): Stats =
  test.toLowerCase match {
    case "autop" => MCDEP(m, alpha, beta, parallelize)
    case "csp" => CSP(m, alpha, beta, parallelize)
    case "cspn" => CSPn(m, alpha, beta, parallelize)
    case "ksp" => KSP(m, alpha, beta, parallelize)
    case "kspn" => KSPn(m, alpha, beta, parallelize)
    case "ksps" => KSPs(m, alpha, beta, parallelize)
    case "kspsn" => KSPsn(m, alpha, beta, parallelize)
    case "mwp" => MWP(m, alpha, beta, parallelize)
    case "mwpn" => MWPn(m, alpha, beta, parallelize)
    case "mwpr" => MWPr(m, alpha, beta, parallelize)
    //case "mwpi" => MWPi(m, alpha, beta, parallelize)
    //case "mwpr" => MWPr(m, alpha, beta, parallelize)
    //case "mwps" => MWPs(m, alpha, beta, parallelize)
    //case "mwpu" => MWPu(m, alpha, beta, parallelize)
    case _ => throw new Error(s"Unknown statistical test $test")
  }
}

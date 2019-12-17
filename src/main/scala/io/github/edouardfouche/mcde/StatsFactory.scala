package io.github.edouardfouche.mcde

object StatsFactory {
  def getTest(test: String, m: Int, alpha: Double, beta: Double, parallelize: Int): Stats =
  test.toLowerCase match {
    case "mcdep" => MCDEP(m, alpha, beta, parallelize)
    case "cspmr" => CSPmr(m, alpha, beta, parallelize)
    case "csp" => CSP(m, alpha, beta, parallelize)
    case "kspemr" => KSPemr(m, alpha, beta, parallelize)
    case "kspe" => KSPe(m, alpha, beta, parallelize)
    case "kspmr" => KSPmr(m, alpha, beta, parallelize)
    case "ksp" => KSP(m, alpha, beta, parallelize)
    case "mwp" => MWP(m, alpha, beta, parallelize)
    case "mwpnomr" => MWPnomr(m, alpha, beta, parallelize)
    case "mwpr" => MWPr(m, alpha, beta, parallelize)
    case "mwpu" => MWPu(m, alpha, beta, parallelize)
    case _ => throw new Error(s"Unknown statistical test $test")
  }
}

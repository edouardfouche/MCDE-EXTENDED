package io.github.edouardfouche.preprocess

// make sure the input of this is col-oriented
class DataSet(val columns: Array[Array[Double]], val types: Array[String]) {
  // supported types: "n" (numeric), "o" (ordinal), "c" (categorical)
  // If one does not precise the type, simply consider them all numerical
  def this(columns: Array[Array[Double]]) = this(columns, columns.map(x => "n"))

  def take(nrows: Int): DataSet = new DataSet(columns.map(x => x.take(nrows)), types)

  def drop(nrows: Int): DataSet = new DataSet(columns.map(x => x.drop(nrows)), types)

  def head: Array[Double] = columns.map(x => x.head)

  def tail: DataSet = new DataSet(columns.map(x => x.tail), types)

  def get_type(x: Int): String = types(x)
  // check that they all have same number of elements
  require{
    val l = columns.head.length
    columns.map(_.length).forall(_ == l)
  }
  def apply(x: Int): Array[Double] = columns(x)
  def transpose: DataSet = {
    // note that we are loosing which types then
    new DataSet(columns.transpose)
  }
  def last: Array[Double] = columns.last
  def ncols: Int = columns.length
  def nrows: Int = columns.head.length
}


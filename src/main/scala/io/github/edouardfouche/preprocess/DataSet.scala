package io.github.edouardfouche.preprocess

// make sure the input of this is col-oriented
class DataSet(columns: List[Vector[String]]) {
  def this(columns: Array[Array[Double]]) = this(columns.map(_.map(_.toString).toVector).toList)
  def this(columns: Array[Array[Int]]) = this(columns.map(_.map(_.toString).toVector).toList)
  def this(columns: Array[Array[_ >: Int with Double with String]]) = this(columns.map(_.map(_.toString).toVector).toList)
  def this(columns: Array[Array[String]]) = this(columns.map(_.toVector).toList)

  // check that they all have same number of elements
  require{
    val l = columns.head.length
    columns.map(_.length).forall(_ == l)
  }
  def apply(x: Int): Vector[String] = columns(x)
  def transpose: DataSet = {
    new DataSet(columns.toVector.transpose.toList)
  }
  def last: Vector[String] = columns.last
  def ncols: Int = columns.length
  def nrows: Int = columns.head.length
}


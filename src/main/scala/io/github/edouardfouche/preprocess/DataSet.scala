package io.github.edouardfouche.preprocess

// make sure the input of this is col-oriented

//TODO: Solve the type problem here
// -> Why not Vector[Vector[String]] or Tuple?
class DataSet(val columns: Array[Array[Double]], val types: Array[String]) {
  // supported types: "n" (numeric), "o" (ordinal), "c" (categorical)
  //def this(columns: List[Vector[String]]) = this(columns,columns.indices.toArray.map(x => "n"))
  //def this(columns: Array[Array[Double]], types: Array[String]) = this(columns.map(_.map(_.toString).toVector).toList, types)

  /*
  for{(t,i) <- types.zipWithIndex} {
    if(t == "c") {
      try{
        columns(i).map(_.toInt)
      } catch {
        case _: NumberFormatException => throw new Error(s"Column $t, was declared as categorical, but mapping to integer type is ambiguous")
      }
    }
  }

   */


  def this(columns: Array[Array[Double]]) = this(columns, columns.map(x => "n"))


  //def this(columns: Array[Array[Int]]) = this(columns.map(_.map(_.toString).toVector).toList)
  //def this(columns: Array[Array[_ >: Int with Double with String]]) = this(columns.map(_.map(_.toString).toVector).toList)

  //def this(columns: Array[Array[String]]) = this(columns.map(_.toVector).toList)
  //def this(columns: Array[Array[String]], types: Array[String]) = this(columns.map(_.toVector).toList, types)

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


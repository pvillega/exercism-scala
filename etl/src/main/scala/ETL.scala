object ETL {
  def transform(input: Map[Int, Seq[String]]): Map[String, Int] =
    input.flatMap{ case(k,v) => v.map(_.toLowerCase -> k).toMap }
}

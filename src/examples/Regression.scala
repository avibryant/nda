package nda

object Regression {
    case class Rows(n: Int) extends Cardinality
    case class Columns(n: Int) extends Cardinality

    def linearRegressionModel = {
        val w = Variable[N[Columns]]("w")
        val x = Variable[N[Columns] By N[Rows]]("x")
        val y = Variable[N[Rows]]("y")

        val predictions: NDA[N[Rows]] = (x * w).sum
        val errors = predictions - y
        val loss = (errors * errors).sumAll
        (x, y, w, loss)
    }

    val rand = new scala.util.Random
    def linearRegression(rows: List[(List[Double], Double)]): Double = {
        val (x, y, w, loss) = linearRegressionModel
        
        val rowWidth = rows.head._1.size
        val wData: Array[Double] = 1.to(rowWidth).toArray.map{i => rand.nextGaussian}
        
        val xData: Array[Double] = rows.map(_._1).reduce(_ ++ _).toArray
        val yData: Array[Double] = rows.map(_._2).toArray

        implicit val nRows = Rows(rows.size)
        implicit val nColumns = Columns(rowWidth)

        val session =
            Session(DoubleEvaluator)
                .feed(x, xData)
                .feed(y, yData)
                .feed(w, wData)

        val initialLoss: Array[Double] = session.run(loss)
        initialLoss(0)
    }

    def main(args: Array[String]) {
        val rows: List[(List[Double], Double)] = ???
        println(linearRegression(rows))
    }
}
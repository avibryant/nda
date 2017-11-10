package nda

object Regression {
    case class Rows(n: Int) extends Dimension
    case class Columns(n: Int) extends Dimension

    def linearRegressionModel = {
        val w = new Variable[Columns]
        val x = new Variable[Columns By Rows]
        val y = new Variable[Rows]

        val predictions: NDA[Rows] = (x * w).sumOuter
        val errors = predictions - y
        val loss: NDA[One] = (errors * errors).sum
        (x, y, w, loss)
    }

    val rand = new scala.util.Random
    def linearRegression(rows: List[(List[Double], Double)], iterations: Int): List[Double] = {        
        val rowWidth = rows.head._1.size
        val wData: Array[Double] = 1.to(rowWidth).toArray.map{i => rand.nextGaussian}
        
        val xData: Array[Double] = rows.map(_._1).reduce(_ ++ _).toArray
        val yData: Array[Double] = rows.map(_._2).toArray

        val (x, y, w, loss) = linearRegressionModel

        implicit val nRows = Rows(rows.size)
        implicit val nColumns = Columns(rowWidth)

        val initialSession =
            Session(ArrayEvaluator)
                .feed(x, xData)
                .feed(y, yData)
                .feed(w, wData)

        val finalSession = optimize(initialSession, w, loss, 0.01, iterations)
        finalSession.run(w).toList
    }

    def optimize[T, X <: Shape](initialSession: Session[T], variable: Variable[X], loss: NDA[One], learningRate: Double, iterations: Int): Session[T] = {
        val grad = Gradient.derive(variable, loss)
        1.to(iterations).foldLeft(initialSession){case (s,_) =>
            val currentLoss = s.run(loss)(0)
            println(s"Loss: $currentLoss")
            s.update(variable, variable - (grad * learningRate))
        }
    }

    def main(args: Array[String]) {
        val rows: List[(List[Double], Double)] =         
            List(
                (List(1.0,2.0,3.0), 3.0),
                (List(-2.0,0.0,6.0), 6.0)
            )

        println(linearRegression(rows, 100))
    }
}
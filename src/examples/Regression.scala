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
        (w, x, y, loss)
    }

    def logisticRegressionModel = {
        val w = new Variable[Columns]
        val x = new Variable[Columns By Rows]
        val y = new Variable[Rows]

        val preds: NDA[Rows] = sigmoid((x * w).sumOuter)
        val probs = (preds * y) + ((Constant(1.0) - preds) * (Constant(1.0) -y))
        val loss: NDA[One] = -(probs.log.sum)
        (w, x, y, loss)        
    }

    def sigmoid[A <: Shape](nda: NDA[A]): NDA[A] =
        (nda.tanh + Constant(1.0)) * Constant(0.5)

    val rand = new scala.util.Random
    def regression(rows: List[(List[Double], Double)], iterations: Int)(model: (Variable[Columns], Variable[Columns By Rows], Variable[Rows], NDA[One])) {        
        val rowWidth = rows.head._1.size
        val wData: Array[Double] = 1.to(rowWidth).toArray.map{i => rand.nextGaussian}
        
        val xData: Array[Double] = rows.map(_._1).reduce(_ ++ _).toArray
        val yData: Array[Double] = rows.map(_._2).toArray

        val (w, x, y, loss) = model

        implicit val nRows = Rows(rows.size)
        implicit val nColumns = Columns(rowWidth)

        val initialSession =
            Session(ArrayEvaluator)
                .feed(x, xData)
                .feed(y, yData)
                .feed(w, wData)

        val finalSession = optimize(initialSession, w, loss, 0.005, iterations)
        println(finalSession.run(w).toList)
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
        val linearRegressionData: List[(List[Double], Double)] =         
            List(
                (List(1.0,2.0,3.0), 3.0),
                (List(-2.0,0.0,6.0), 6.0),
                (List(1.0,2.0,9.0), 9.0)
            )

        regression(linearRegressionData, 1000)(linearRegressionModel)

        val logisticRegressionData: List[(List[Double], Double)] =         
            List(
                (List(0.52, 1.12,  0.77), 1.0),
                (List(0.88, -1.08, 0.15), 1.0),
                (List(0.52, 0.06, -1.30), 0.0),
                (List(0.74, -2.49, 1.39), 1.0)
            )

  //      regression(logisticRegressionData, 1000)(logisticRegressionModel)
    }
}

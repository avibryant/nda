package nda

object Regression {
    case class Rows(n: Int) extends Cardinality
    case class Columns(n: Int) extends Cardinality

    def linearRegressionModel = {
        val w = Variable[N[Columns]]("w")
        val x = Variable[N[Columns] By N[Rows]]("x")
        val y = Variable[N[Rows]]("y")

        val predictions: NDA[N[Rows]] = (x * w).sumOuter
        val errors = predictions - y
        val loss: NDA[One] = (errors * errors).sum
        (x, y, w, loss)
    }

    val rand = new scala.util.Random
    def linearRegression(rows: List[(List[Double], Double)]) {
        val (x, y, w, loss) = linearRegressionModel
        val wGrad = Gradient.derive(w, loss)
        
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

        1.to(10).foldLeft((session,wData)){case ((s,prevW),_) =>
            println(prevW.toList)
            val currentLoss = s.run(loss)(0)
            println(currentLoss)
            val gradient = s.run(wGrad)
            val newW = prevW.zip(gradient).map{case (l,r) => l - (r * 0.01)}
            (s.feed(w, newW), newW)
        }
    }

    def main(args: Array[String]) {
        val rows: List[(List[Double], Double)] =         
            List(
                (List(1.0,2.0,3.0), 3.0),
                (List(2.0,4.0,6.0), 6.0)
            )

        linearRegression(rows)
    
        println("----")

        val a = Variable[N[Columns]]("a")
        val loss: NDA[One] = (a * a).sum
        val aGrad = Gradient.derive(a, loss)

        implicit val r = Columns(3)
        val session =
            Session(DoubleEvaluator)
                .feed(a, Array(1.0, 2.0, 3.0))
        
        println(aGrad)
        println(session.run(loss)(0))
        println(session.run(aGrad).toList)
    }
}
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

        1.to(100).foldLeft((session,wData)){case ((s,prevW),_) =>
            println(prevW.toList)
            val currentLoss = s.run(loss)(0)
            println(currentLoss)
            println(s.run((x * w).sumOuter).toList)
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
    /*
        println("----")

        val a = Variable[N[Columns]]("a")
        val b = Variable[One]("b")
        val c = a.sum[One] - b
        val loss: NDA[One] = c * c
        val aGrad = Gradient.derive(a, loss)
        
        val aData = Array(1.0, 2.0, 3.0)
        val bData = Array(10.0)
        implicit val co = Columns(aData.size / bData.size)
        implicit val ro = Rows(bData.size)
        val session =
            Session(DoubleEvaluator)
                .feed(a, aData)
                .feed(b, bData)

        1.to(10).foldLeft((session,aData)){case ((s,prevA),_) =>
           // println(prevA.toList)
            println(s.run(c).toList)
            println(s.run(Gradient.derive(c,loss)).toList)
            println(s.run(Gradient.derive(a.sum[One],loss)).toList)
            val currentLoss = s.run(loss)(0)
           // println(currentLoss)
            val gradient = s.run(aGrad)
            val newA = prevA.zip(gradient).map{case (l,r) => l - (r * 0.1)}
            (s.feed(a, newA), newA)
        }*/
    }
}
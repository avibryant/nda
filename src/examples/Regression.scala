package nda

object Regression {
    case class Rows()
    case class Columns()

    def linearRegressionError(w: NDA[N[Columns]], x: NDA[N[Rows] By N[Columns]], y: NDA[N[Rows]]): NDA[One] = {
        val predictions: NDA[N[Rows]] = (x * w).sum
        val errors = predictions - y
        (errors * errors).sum
    }
}
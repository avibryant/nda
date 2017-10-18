package nda

object Regression {
    case class Rows()
    case class Columns()

    def linearRegressionError(w: NDA[N[Columns]], xs: NDA[N[Rows] By N[Columns]], ys: NDA[N[Rows]]): NDA[One] = {
        val predictions: NDA[N[Rows]] = (xs * w).sum
        val errors = predictions - ys
        (errors * errors).sum
    }
}
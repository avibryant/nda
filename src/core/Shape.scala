package nda

sealed trait Shape

trait Dimension extends Shape {
    def n: Int
}
case class One() extends Dimension {
    val n = 1
}

case class By[D<:Dimension,X<:Shape]() extends Shape
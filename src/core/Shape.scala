package nda

sealed trait Shape {
    def toList: List[Int]
}

trait Dimension extends Shape {
    def n: Int
    def toList = List(n)
}

case class One() extends Dimension {
    val n = 1
}

case class By[D<:Dimension,X<:Shape](outer: D, inner: X) extends Shape {
    def toList = outer.toList ++ inner.toList
}

object Shape {
    implicit val one: One = One()
        
    implicit def by[D <: Dimension,X <: Shape](implicit outer: D, inner: X): By[D,X] = By(outer, inner)
}
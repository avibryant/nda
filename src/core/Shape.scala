package nda

sealed trait Shape

sealed trait Dimension extends Shape

case class One() extends Dimension {
    def by[X<:Shape](inner: X) = By(this, inner)
}

case class N[A]() extends Dimension

case class By[D<:Dimension,X<:Shape](outer: D, inner: X) extends Shape

object Shape {
    def broadcast[X <: Shape,Y <: Shape,Z <: Shape](x: X, y: Y)(implicit b: Broadcast[X,Y,Z]): Z
        = b(x,y)

    implicit def one: One = One()
    implicit def n[A]: N[A] = N[A]()
    implicit def by[D<:Dimension,X<:Shape](implicit outer: D, inner:X): By[D,X] = By(outer, inner)
}
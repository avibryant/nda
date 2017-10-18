package nda

sealed trait Shape
sealed trait Dimension extends Shape

case class One() extends Dimension {
    def by[X<:Shape](inner: X) = By(this, inner)
}

case class N[A]() extends Dimension {
    def by[X<:Shape](inner: X) = By(this, inner)
}

case class By[D<:Dimension,X<:Shape](outer: D, inner: X) extends Shape

object Shape {
    def broadcast[X <: Shape,Y <: Shape,Z <: Shape](x: X, y: Y)(implicit b: Broadcast[X,Y,Z]): Z
        = b(x,y)

    def newAxis[X <: Shape, Z <: Shape](x: X)(implicit e: Expand[X,One,Z]): Z
        = e.expand(x, One())
    
    def compact[X <: Shape, Y <: Dimension, Z <: Shape](z: Z)(implicit e: Expand[X,Y,Z]): X
        = e.compact(z)
}
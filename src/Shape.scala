package nda

sealed trait Shape
sealed trait Dimension extends Shape

case class One() extends Dimension {
    def by[X<:Shape](inner: X) = By(this, inner)
    def newAxis = by(One())
}

case class N[A]() extends Dimension {
    def by[X<:Shape](inner: X) = By(this, inner)
    def newAxis = by(One())
}

case class By[D<:Dimension,X<:Shape](outer: D, inner: X) extends Shape {
    def newAxis[Z <: Shape](implicit n: Shape.NewAxis[X,Z]): By[D,Z] =
        By(outer, n(inner))
}

object Shape {
    def broadcast[X <: Shape,Y <: Shape,Z <: Shape](x: X, y: Y)(implicit b: Broadcast[X,Y,Z]): Z
        = b(x,y)

    trait NewAxis[X <: Shape, Z <: Shape] {
        def apply(x: X): Z
    }

    object NewAxis {
        implicit def one: NewAxis[One, By[One,One]] = new NewAxis[One, By[One,One]] {
            def apply(x: One) = x.by(One())
        }

        implicit def n[A]: NewAxis[N[A], By[N[A],One]] = new NewAxis[N[A], By[N[A],One]] {
            def apply(x: N[A]) = x.by(One())
        }

        implicit def by[D <: Dimension, X <: Shape, Z <: Shape](
            implicit innerNewAxis: NewAxis[X,Z]
        ) = new NewAxis[By[D,X], By[D,Z]] {
            def apply(x: By[D,X]) = By(x.outer, innerNewAxis(x.inner))
        }
    }
}
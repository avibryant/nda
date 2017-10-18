package nda

trait Expand[X <: Shape, Y <: Dimension, Z <: Shape] {
    def expand(x: X, y: Y): Z
    def compact(z: Z): X
}

trait LowPriorityExpand {
    implicit def by[D <: Dimension, X <: Shape, Y <: Dimension, Z <: Shape](
        implicit innerExpand: Expand[X,Y,Z]
    ) = new Expand[By[D,X], Y, By[D,Z]] {
        def expand(x: By[D,X], y: Y) = By(x.outer, innerExpand.expand(x.inner, y))
        def compact(z: By[D,Z]) = By(z.outer, innerExpand.compact(z.inner))
    }

    implicit def one[Z <: Dimension]: Expand[One, Z, Z] = new Expand[One, Z, Z] {
        def expand(x: One, y: Z) = y
        def compact(z: Z) = One()
    }
}

object Expand extends LowPriorityExpand {
    implicit def dimension[X <: Dimension, Y <: Dimension] = new Expand[X, Y, By[X,Y]] {
        def expand(x: X, y: Y) = By(x, y)
        def compact(z: By[X,Y]) = z.outer
    }
}
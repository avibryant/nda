package nda

trait Broadcast[X <: Shape,Y <: Shape,Z <: Shape] {
    def apply(x: X, y: Y): Z
}

trait BroadcastLowPriority {
    implicit def one2one = new Broadcast[One,One,One] {
        def apply(x: One, y: One) = x
    }

    implicit def one2n[A] = new Broadcast[One,N[A],N[A]] {
        def apply(x: One, y: N[A]) = y
    }

    implicit def n2one[A] = new Broadcast[N[A],One,N[A]] {
        def apply(x: N[A], y: One) = x
    }

    implicit def n2n[A] = new Broadcast[N[A],N[A],N[A]] {
        def apply(x: N[A], y: N[A]) = x
    }
    
    implicit def dim2By[X <: Dimension, Y <: Dimension, Z <: Dimension, B <: Shape](
        implicit outerBroadcast: Broadcast[X, Y, Z]
    ) = new Broadcast[X, By[Y,B], By[Z,B]] {
        def apply(x: X, y: By[Y,B]) = By(outerBroadcast(x, y.outer), y.inner)
    }

    implicit def by2Dim[X <: Dimension, Y <: Dimension, Z <: Dimension, B <: Shape](
        implicit outerBroadcast: Broadcast[X, Y, Z]
    ) = new Broadcast[By[X,B], Y, By[Z,B]] {
        def apply(x: By[X,B], y: Y) = By(outerBroadcast(x.outer, y), x.inner)
    }
}

object Broadcast extends BroadcastLowPriority {
    implicit def outerInner[X <: Dimension, Y <: Dimension, Z <: Dimension, B<:Shape, C<:Shape, D<:Shape](
        implicit outerBroadcast: Broadcast[X,Y,Z],
        innerBroadcast: Broadcast[B,C,D]        
    ) = new Broadcast[By[X,B],By[Y,C],By[Z,D]] {
        def apply(x: By[X,B], y: By[Y,C]) =
            By(outerBroadcast(x.outer,y.outer), innerBroadcast(x.inner,y.inner))
    }    
}

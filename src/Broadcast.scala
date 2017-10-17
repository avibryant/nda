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

    implicit def leftInner[X <: Dimension, B<:Shape, C<:Shape, D<:Shape](
        implicit innerBroadcast: Broadcast[B,C,D]        
    ) = new Broadcast[By[X,B], C, By[X,D]] {
        def apply(x: By[X,B], y: C) = By(x.outer, innerBroadcast(x.inner,y))
    }

    implicit def rightInner[X <: Dimension, B<:Shape, C<:Shape, D<:Shape](
        implicit innerBroadcast: Broadcast[B,C,D]        
    ) = new Broadcast[B,By[X,C],By[X,D]] {
        def apply(x: B, y: By[X,C]) = By(y.outer, innerBroadcast(x,y.inner))
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

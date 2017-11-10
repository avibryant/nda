package nda

//here be dragons

trait Broadcaster[X <: Shape,Y <: Shape,Z <: Shape] {
    def combine(left: List[Int], right: List[Int]): List[Int]
    def left(combined: List[Int]): List[Int]
    def right(combined: List[Int]): List[Int]
    def swap: Broadcaster[Y,X,Z] = SwapBroadcaster(this) 
    def rightResult: Broadcaster[Z,Y,Z] = RightBroadcaster(this)
    def leftResult: Broadcaster[Z,X,Z] = swap.rightResult
}

case class SwapBroadcaster[X <: Shape,Y <: Shape,Z <: Shape](original: Broadcaster[Y,X,Z])
    extends Broadcaster[X,Y,Z] {
    
    def combine(left: List[Int], right: List[Int]) = original.combine(right, left)
    def left(combined: List[Int]) = original.right(combined)
    def right(combined: List[Int]) = original.left(combined)
}

case class RightBroadcaster[X <: Shape, Y <: Shape, Z <: Shape](original: Broadcaster[Y,X,Z])
    extends Broadcaster[Z,X,Z] {

    def combine(left: List[Int], right: List[Int]) = left
    def left(combined: List[Int]) = combined
    def right(combined: List[Int]) = original.right(combined)
}

trait BroadcasterLowestPriority {
    implicit def one2Any[X<:Shape] = new Broadcaster[One,X,X]{
        def combine(left: List[Int], right: List[Int]) = right
        def left(combined: List[Int]) = List(1)
        def right(combined: List[Int]) = combined
    }

    implicit def any2one[X<:Shape] = one2Any[X].swap
}

trait BroadcasterLowPriority extends BroadcasterLowestPriority {
    implicit def any2any[X <: Shape] = new Broadcaster[X,X,X] {
        def combine(left: List[Int], right: List[Int]) = left
        def left(combined: List[Int]) = combined
        def right(combined: List[Int]) = combined       
    }
    
    implicit def dim2By[X <: Dimension, Y <: Dimension, Z <: Dimension, B <: Shape](
        implicit outerBroadcaster: Broadcaster[X, Y, Z]
    ) = new Broadcaster[X, By[Y,B], By[Z,B]] {
        def combine(left: List[Int], right: List[Int]) = 
            outerBroadcaster.combine(left, right.take(1)) ++ right.tail
        def left(combined: List[Int]) = outerBroadcaster.left(combined.take(1))
        def right(combined: List[Int]) = outerBroadcaster.right(combined.take(1)) ++ combined.tail
    }

    implicit def by2Dim[X <: Dimension, Y <: Dimension, Z <: Dimension, B <: Shape](
        implicit outerBroadcaster: Broadcaster[X, Y, Z]
    ) = dim2By[Y,X,Z,B](outerBroadcaster.swap).swap
}

object Broadcaster extends BroadcasterLowPriority {
    implicit def outerInner[X <: Dimension, Y <: Dimension, Z <: Dimension, B<:Shape, C<:Shape, D<:Shape](
        implicit outerBroadcaster: Broadcaster[X,Y,Z],
        innerBroadcaster: Broadcaster[B,C,D]        
    ) = new Broadcaster[By[X,B],By[Y,C],By[Z,D]] {
        def combine(left: List[Int], right: List[Int]) = 
            outerBroadcaster.combine(left.take(1), right.take(1)) ++ innerBroadcaster.combine(left.tail, right.tail)
        def left(combined: List[Int]) = outerBroadcaster.left(combined.take(1)) ++ innerBroadcaster.left(combined.tail)
        def right(combined: List[Int]) = outerBroadcaster.right(combined.take(1)) ++ innerBroadcaster.right(combined.tail)       
    }
}

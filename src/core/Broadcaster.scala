package nda

//here be dragons

trait Broadcaster[X <: Shape,Y <: Shape,Z <: Shape] {
    def apply(left: List[Int], right: List[Int]): List[Int]
    def swap: Broadcaster[Y,X,Z] = SwapBroadcaster(this) 
    def leftReducer: Reducer[Z,X]
    def rightReducer: Reducer[Z,Y]
}

case class SwapBroadcaster[X <: Shape,Y <: Shape,Z <: Shape](original: Broadcaster[Y,X,Z])
    extends Broadcaster[X,Y,Z] {
    
    def apply(left: List[Int], right: List[Int]) = original(right, left)
    val leftReducer = original.rightReducer
    val rightReducer = original.leftReducer
}

trait BroadcasterLowestPriority {
    implicit def one2Any[X<:Shape] = new Broadcaster[One,X,X]{
        def apply(left: List[Int], right: List[Int]) = right
        val leftReducer = new Reducer[X, One] {
            def apply(size: List[Int]) = List(1)
        }
        val rightReducer = Reducer.identity[X]
    }

  implicit def swap[X <: Shape, Y <: Shape, Z <: Shape](implicit b: Broadcaster[X,Y,Z]): Broadcaster[Y,X,Z] = b.swap
}

trait BroadcasterLowPriority extends BroadcasterLowestPriority {
    implicit def any2any[X <: Shape] = new Broadcaster[X,X,X] {
        def apply(left: List[Int], right: List[Int]) = left
        def leftReducer = Reducer.identity[X]
        def rightReducer = Reducer.identity[X]
    }
    
    implicit def dim2By[X <: Dimension, Y <: Dimension, Z <: Dimension, B <: Shape](
        implicit outerBroadcaster: Broadcaster[X, Y, Z]
    ) = new Broadcaster[X, By[Y,B], By[Z,B]] {
        def apply(left: List[Int], right: List[Int]) = 
            outerBroadcaster(left, right.take(1)) ++ right.tail
        def leftReducer = new Reducer[By[Z,B],X] {
            def apply(size: List[Int]) = outerBroadcaster.leftReducer(size.take(1))
        }
        def rightReducer = new Reducer[By[Z,B],By[Y,B]] {
            def apply(size: List[Int]) = outerBroadcaster.rightReducer(size.take(1)) ++ size.tail
        }
    }
}

object Broadcaster extends BroadcasterLowPriority {
    implicit def outerInner[X <: Dimension, Y <: Dimension, Z <: Dimension, B<:Shape, C<:Shape, D<:Shape](
        implicit outerBroadcaster: Broadcaster[X,Y,Z],
        innerBroadcaster: Broadcaster[B,C,D]        
    ) = new Broadcaster[By[X,B],By[Y,C],By[Z,D]] {
        def apply(left: List[Int], right: List[Int]) = 
            outerBroadcaster(left.take(1), right.take(1)) ++ innerBroadcaster(left.tail, right.tail)
        def leftReducer = new Reducer[By[Z,D],By[X,B]] {
            def apply(size: List[Int]) = outerBroadcaster.leftReducer(size.take(1)) ++ innerBroadcaster.leftReducer(size.tail)
        }
        def rightReducer = new Reducer[By[Z,D], By[Y,C]] {
            def apply(size: List[Int]) = outerBroadcaster.rightReducer(size.take(1)) ++ innerBroadcaster.rightReducer(size.tail)
        }
    }
}
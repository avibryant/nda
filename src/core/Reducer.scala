package nda

trait Reducer[X <: Shape, Y <: Shape] {self =>
    def apply(size: List[Int]): List[Int]
    def toBroadcaster: Broadcaster[X, Y, X] = new Broadcaster[X, Y, X] {
        def apply(left: List[Int], right: List[Int]) = left
        def leftReducer = Reducer.identity[X]
        def rightReducer = self
    }
}

object Reducer {
    implicit def dim[X <: Dimension] = new Reducer[X,One] {
        def apply(size: List[Int]) = List(1)
    }

    implicit def by[X <: Dimension, Y <: Shape] = new Reducer[By[X,Y], By[One,Y]] {
        def apply(size: List[Int]) = 1 :: size.tail
    }

    def identity[X <: Shape] = new Reducer[X, X] {
        def apply(size: List[Int]) = size
    }
}
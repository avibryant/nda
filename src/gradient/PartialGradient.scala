package nda

trait PartialGradient[X <: Shape] {
    def toNDA: NDA[X]
}

case class PGBinary[X <: Shape, Y <: Shape, Z <: Shape](child: Binary[X,Y,Z], gradient: Gradient[Z])
    extends PartialGradient[X] {
        def toNDA = {
            implicit val red = child.broadcaster.leftReducer
            child.op match {
                case AddOp => gradient.toNDA.sum[X]
                case MultiplyOp => 
                    implicit val bcast = child.broadcaster.rightReducer.toBroadcaster
                    val weightedGrad = gradient.toNDA * child.right
                    weightedGrad.sum[X]
            }
        }
}

case class PGUnary[X <: Shape](child: Unary[X], gradient: Gradient[X]) extends PartialGradient[X]  {
        def toNDA = child.op match {
            case IdentityOp => child * 0.0
        }
}

case class PGReduce[X <: Shape, Y <: Shape](child: Reduce[X,Y], gradient: Gradient[Y]) extends PartialGradient[X]  {
        def toNDA = child.op match {
            case AddOp =>
                implicit val b: Broadcaster[X,Y,X] = child.reducer.toBroadcaster
                child.original.identity * gradient.toNDA
            case MultiplyOp => ???
        }
}

case class PGNewAxis[X <: Shape](child: NewAxis[X], gradient: Gradient[By[One,X]]) extends PartialGradient[X]  {
        def toNDA = gradient.toNDA.dropAxis
}

case class PGDropAxis[X <: Shape](child: DropAxis[X], gradient: Gradient[X]) extends PartialGradient[By[One,X]] {
        def toNDA = gradient.toNDA.newAxis
}

case object PGOne extends PartialGradient[One] {
    val toNDA = Constant(1.0)
}
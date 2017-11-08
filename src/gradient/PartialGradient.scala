package nda

trait Gradient[X <: Shape] {
    def toNDA: NDA[X]
}

trait PartialGradient[X <: Shape] {
    def toNDA: NDA[X]
}

case class PGLeftBinary[X <: Shape, Y <: Shape, Z <: Shape](child: Binary[X,Y,Z], gradient: Gradient[Z])
    extends PartialGradient[X] {
        def toNDA = child.op match {
            case AddOp => Reduce(gradient.toNDA, AddOp)(child.b)
            case MultiplyOp => 
                implicit val b: Broadcaster[Z, Y, Z] = child.b.rightResult
                Reduce(gradient.toNDA * child.right, AddOp)(child.b)
        }
}

case class PGRightBinary[X <: Shape, Y <: Shape, Z <: Shape](child: Binary[X,Y,Z], gradient: Gradient[Z])
    extends PartialGradient[Y]  {
        def toNDA = child.op match {
            case AddOp => Reduce(gradient.toNDA, AddOp)(child.b.swap)
            case MultiplyOp => 
                implicit val b: Broadcaster[Z, X, Z] = child.b.leftResult
                Reduce(gradient.toNDA * child.left, AddOp)(child.b.swap)
        }
}

case class PGUnary[X <: Shape](child: Unary[X], gradient: Gradient[X]) extends PartialGradient[X]  {
        def toNDA = ???
}

case class PGReduce[X <: Shape, Y <: Shape](child: Reduce[X,Y], gradient: Gradient[Y]) extends PartialGradient[X]  {
        def toNDA = child.op match {
            case AddOp => ???
            case MultiplyOp => ???
        }
}

case class PGNewAxis[X <: Shape](child: NewAxis[X], gradient: Gradient[By[One,X]]) extends PartialGradient[X]  {
        def toNDA = ???
}

case object PGOne extends PartialGradient[One] {
    val toNDA = Constant(1.0)
}
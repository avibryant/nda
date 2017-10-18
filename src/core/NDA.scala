package nda

sealed trait NDA[X<:Shape] {
    def shape: X
    def +[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, AddOp)
    def -[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, SubtractOp)
    def *[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, MultiplyOp)
//    def newAxis[Z <: Shape](implicit e: Expand[X,One,Z]): NDA[Z] =
//        NewAxis(this)
    def sum[Y <: Dimension, Z <: Shape](implicit e:Expand[Z,Y,X]): NDA[Z] =
        Reduce(this, AddOp)
    def product[Y <: Dimension, Z <: Shape](implicit e:Expand[Z,Y,X]): NDA[Z] =
        Reduce(this, MultiplyOp)
}

case class Scalar(value: Double) extends NDA[One] {
    val shape = One()
}

case class Vector[A](values: Array[Double]) extends NDA[N[A]] {
    val shape = N[A]()
}

case class Binary[X<:Shape, Y<:Shape, Z<:Shape](left: NDA[X], right: NDA[Y], op: BinaryOp)(
    implicit b: Broadcast[X,Y,Z]
) extends NDA[Z] {
    val shape = b(left.shape, right.shape)
}

case class Unary[X<:Shape](original: NDA[X], op: UnaryOp) extends NDA[X] {
    val shape = original.shape
}

case class Reduce[X<:Shape,Y<:Dimension,Z<:Shape](original: NDA[Z], op: AssociativeOp)(
    implicit e: Expand[X,Y,Z]
) extends NDA[X] {
    val shape = e.compact(original.shape)
}
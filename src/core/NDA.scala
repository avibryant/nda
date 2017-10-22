package nda

sealed trait NDA[X<:Shape] {
    def shape: X
    def +[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, AddOp)
    def -[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, SubtractOp)
    def *[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcast[X,Y,Z]): NDA[Z] =
        Binary(this, other, MultiplyOp)
    def newAxis: NDA[By[One,X]] =
        NewAxis(this)
    def sum[Y <: Shape](implicit ev: X <:< By[_,Y]): NDA[Y] =
        Reduce(this, AddOp)
    def product[Y <: Shape](implicit ev: X <:< By[_,Y]): NDA[Y] =
        Reduce(this, MultiplyOp)
    def sumAll: NDA[One] = ReduceAll(this, AddOp)
    def productAll: NDA[One] = ReduceAll(this, MultiplyOp)
}

case class Variable[X <: Shape](name: String)(implicit val shape: X) extends NDA[X]

case class Binary[X<:Shape, Y<:Shape, Z<:Shape](left: NDA[X], right: NDA[Y], op: BinaryOp)(
    implicit b: Broadcast[X,Y,Z]
) extends NDA[Z] {
    val shape = b(left.shape, right.shape)
}

case class Unary[X<:Shape](original: NDA[X], op: UnaryOp) extends NDA[X] {
    val shape = original.shape
}

case class Reduce[X<:Shape,Y<:Shape](original: NDA[X], op: AssociativeOp)(
    implicit ev: X <:< By[_,Y]
) extends NDA[Y] {
    val shape = ev(original.shape).inner
}

case class ReduceAll[X<:Shape](original: NDA[X], op: AssociativeOp) extends NDA[One] {
    val shape = One()
}

case class NewAxis[X<:Shape](original: NDA[X]) extends NDA[By[One,X]] {
    val shape = One().by(original.shape)
}
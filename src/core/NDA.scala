package nda

sealed trait NDA[X<:Shape] {
    def +[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcaster[X,Y,Z]): NDA[Z] =
        Binary(this, other, AddOp, b)
    def -[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcaster[X,Y,Z]): NDA[Z] = {
        val negated: NDA[Y] = other * Constant(-1)
        this + negated
    }
    def *[Y<:Shape, Z<:Shape](other: NDA[Y])(implicit b: Broadcaster[X,Y,Z]): NDA[Z] =
        Binary(this, other, MultiplyOp, b)
    def dropAxis[Y <: Shape](implicit ev: X <:< By[One,Y]): NDA[Y] =
        DropAxis(this.asInstanceOf[NDA[By[One,Y]]])
    def newAxis: NDA[By[One,X]] =
        NewAxis(this)
    def sum[Y <: Shape](implicit b: Broadcaster[Y,X,X]): NDA[Y] =
        Reduce(this, AddOp)
    def sumOuter[Y <: Shape](implicit b: Broadcaster[By[One,Y],X,X]): NDA[Y] =
        sum[By[One,Y]].dropAxis
}

case class Variable[X <: Shape](name: String) extends NDA[X]

case class Constant(value: Double) extends NDA[One]

case class Binary[X<:Shape, Y<:Shape, Z<:Shape](left: NDA[X], right: NDA[Y], op: BinaryOp, b: Broadcaster[X,Y,Z])
    extends NDA[Z]

case class Unary[X<:Shape](original: NDA[X], op: UnaryOp) extends NDA[X]

case class Reduce[X<:Shape,Y<:Shape](original: NDA[X], op: BinaryOp)(implicit val b: Broadcaster[Y,_,X])
    extends NDA[Y]

case class NewAxis[X<:Shape](original: NDA[X]) extends NDA[By[One,X]]
case class DropAxis[X<:Shape](original: NDA[By[One,X]]) extends NDA[X]
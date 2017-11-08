package nda

sealed trait Shape

sealed trait Dimension extends Shape
case class One() extends Dimension
case class N[A]() extends Dimension
case class By[D<:Dimension,X<:Shape]() extends Shape

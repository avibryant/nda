package nda

sealed trait BinaryOp
case object AddOp extends BinaryOp
case object MultiplyOp extends BinaryOp

sealed trait UnaryOp
case object MinusOp extends UnaryOp
case object IdentityOp extends UnaryOp
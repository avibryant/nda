package nda

sealed trait BinaryOp
case object AddOp extends BinaryOp
case object MultiplyOp extends BinaryOp

sealed trait UnaryOp
case object IdentityOp extends UnaryOp
case object TanhOp extends UnaryOp
case object LogOp extends UnaryOp
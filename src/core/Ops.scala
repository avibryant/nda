package nda

sealed trait BinaryOp

case object SubtractOp extends BinaryOp

sealed trait AssociativeOp extends BinaryOp
case object AddOp extends AssociativeOp
case object MultiplyOp extends AssociativeOp

sealed trait UnaryOp
case object LogOp extends UnaryOp
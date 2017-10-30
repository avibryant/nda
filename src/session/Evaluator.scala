package nda

trait Evaluator[T] {
    def binary(left: ShapedArray[T], right: ShapedArray[T], op: BinaryOp): ShapedArray[T]
    def unary(original: ShapedArray[T], op: UnaryOp): ShapedArray[T]
    def reduce(original: ShapedArray[T], op: AssociativeOp): ShapedArray[T]
    def reduceAll(original: ShapedArray[T], op: AssociativeOp): ShapedArray[T]
}
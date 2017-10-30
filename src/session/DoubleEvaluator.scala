package nda

object DoubleEvaluator extends Evaluator[Double] {
    def binary(left: ShapedArray[Double], right: ShapedArray[Double], op: BinaryOp) = ???
    def unary(original: ShapedArray[Double], op: UnaryOp) = ???
    def reduce(original: ShapedArray[Double], op: AssociativeOp) = ???
    def reduceAll(original: ShapedArray[Double], op: AssociativeOp) = ???
}
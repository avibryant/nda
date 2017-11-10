package nda

trait Evaluator[T] {
    def in(size: List[Int], array: Array[Double]): T
    def out(t: T): Array[Double]
    def size(t: T): List[Int]
    def alloc(shape: List[Int]): T
    def binary(left: T, right: T, result: T, op: BinaryOp): Unit
    def unary(original: T, result: T, op: UnaryOp): Unit
    def reduce(original: T, result: T, op: BinaryOp): Unit
    def newAxis(original: T): T
    def dropAxis(original: T): T
}
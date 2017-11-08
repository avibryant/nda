package nda

case class ShapedArray[T](size: List[Int], array: Array[T]) {
    def newAxis: ShapedArray[T] = ShapedArray(1 :: size, array)
    def dropAxis: ShapedArray[T] = ShapedArray(size.tail, array)
}

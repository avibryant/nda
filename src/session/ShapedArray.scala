package nda

case class ShapedArray[T](size: List[Int], array: Array[T]) {
    def newAxis: ShapedArray[T] = ???
}

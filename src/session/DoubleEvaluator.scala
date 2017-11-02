package nda

object DoubleEvaluator extends Evaluator[Double] {
    def binary(left: ShapedArray[Double], right: ShapedArray[Double], op: BinaryOp) = {
        val resultRank = left.size.size.max(right.size.size)
        val leftSize = pad(left.size, resultRank)
        val rightSize = pad(right.size, resultRank)
        val resultSize = leftSize.zip(rightSize).map{case (l,r) => l.max(r)}

        val result = alloc(resultSize.toList)
        val resultArray = result.array
        val leftArray = left.array
        val rightArray = right.array

        var i = resultArray.size - 1

        op match {
            case AddOp =>
                while(i >= 0) {
                    val leftIndex = computeIndex(i, resultSize, leftSize)
                    val rightIndex = computeIndex(i, resultSize, rightSize)
                    resultArray(i) = leftArray(leftIndex) + rightArray(rightIndex)
                    i -= 1
                }
            case MultiplyOp =>
                while(i >= 0) {
                    val leftIndex = computeIndex(i, resultSize, leftSize)
                    val rightIndex = computeIndex(i, resultSize, rightSize)
                    resultArray(i) = leftArray(leftIndex) * rightArray(rightIndex)
                    i -= 1
                }
            case SubtractOp =>
                while(i >= 0) {
                    val leftIndex = computeIndex(i, resultSize, leftSize)
                    val rightIndex = computeIndex(i, resultSize, rightSize)
                    resultArray(i) = leftArray(leftIndex) - rightArray(rightIndex)
                    i -= 1
                }
        }
        result
    }

    private def computeIndex(i: Int, resultSize: Array[Int], size: Array[Int]): Int = {
        var j = i
        var k = 0
        var l = 1
        var a = 0

        while(a < resultSize.size) {
            val rsA = resultSize(a)
            val sA = size(a)
            k += (j % rsA % sA)  * l
            l *= sA 
            j /= rsA
            a += 1
        }
        k
    }

    def unary(original: ShapedArray[Double], op: UnaryOp) = {
        val result = alloc(original.size)
        val originalArray = original.array
        val resultArray = result.array
        var i = originalArray.size - 1
        op match {
            case LogOp =>
                while(i >= 0) {
                    resultArray(i) = Math.log(originalArray(i))
                    i -= 1
                }
        }
        result
    }

    def reduce(original: ShapedArray[Double], op: AssociativeOp) = {
        val newSize = original.size.tail
        val result = alloc(newSize)
        val period = original.size.head
        val originalArray = original.array
        var i = originalArray.size - 1
        while(i >= 0) {
            var acc = zero(op)
            val nextPeriod = i - period
            op match {
                case AddOp =>
                    while(i > nextPeriod) {
                        acc += originalArray(i)
                        i -= 1
                    }
                case MultiplyOp =>
                    while(i > nextPeriod) {
                        acc *= originalArray(i)
                        i -= 1
                    }
            }
            result.array((i+1)/period) = acc
        }
        result
    }

    def reduceAll(original: ShapedArray[Double], op: AssociativeOp) = {
        var result = zero(op)
        val originalArray = original.array
        var i = originalArray.size - 1
        op match {
            case AddOp =>
                while(i >= 0) {
                    result += originalArray(i)
                    i -= 1
                }
            case MultiplyOp =>
                while(i >= 0) {
                    result *= originalArray(i)
                    i -= 1
                }
        }
        ShapedArray(List(1), Array(result))
    }


    private def zero(op: AssociativeOp): Double = op match {
        case AddOp => 0.0
        case MultiplyOp => 1.0
    }

    private def alloc(size: List[Int]) =
        ShapedArray(size, Array.ofDim[Double](size.reduce(_ * _)))

    private def pad(size: List[Int], rank: Int): Array[Int] = 
        (size ++ List.fill(rank - size.size)(1)).toArray
}
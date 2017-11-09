package nda

//this (and only this) is written with performance as the top priority
//I'm probably making all kinds of incorrect assumptions about what will or won't be fast
//(eg about what I can expect to be inlined, etc)
object DoubleEvaluator extends Evaluator[Double] {
    def constant(value: Double) = ShapedArray(List(1), Array(value))

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
        }
        result
    }

    //hope this inlines
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
            case MinusOp =>
                while(i >= 0) {
                    resultArray(i) = originalArray(i) * -1
                    i -= 1
                }
            case IdentityOp =>
                while(i >= 0) {
                    resultArray(i) = 1.0
                    i -= 1
                }                
        }
        result
    }

    def reduce(original: ShapedArray[Double], op: BinaryOp, newSize: List[Int]) = {
        val result = alloc(newSize)
        val resultArray = result.array
        val resultSize = pad(result.size, original.size.size).toArray
        val originalArray = original.array
        val originalSize = original.size.toArray
        var i = originalArray.size - 1
        op match {
            case AddOp =>
                while(i > 0) {
                    val resultIndex = computeIndex(i, originalSize, resultSize)
                    resultArray(resultIndex) += originalArray(i)
                    i -= 1
                }
            case MultiplyOp => ???
        }
        result
    }

    private def zero(op: BinaryOp): Double = op match {
        case AddOp => 0.0
        case MultiplyOp => 1.0
    }

    private def alloc(size: List[Int]) =
        ShapedArray(size, Array.ofDim[Double](size.reduce(_ * _)))

    private def pad(size: List[Int], rank: Int): Array[Int] = 
        (size ++ List.fill(rank - size.size)(1)).toArray
}
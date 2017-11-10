package nda

case class ShapedArray(size: List[Int], array: Array[Double])

//this (and only this) is written with performance as the top priority
//I'm probably making all kinds of incorrect assumptions about what will or won't be fast
//(eg about what I can expect to be inlined, etc)
object ArrayEvaluator extends Evaluator[ShapedArray] {
    def in(size: List[Int], array: Array[Double]) = ShapedArray(size, array)
    def out(t: ShapedArray) = t.array
    def size(t: ShapedArray) = t.size

    def alloc(size: List[Int]): ShapedArray =
        ShapedArray(size, Array.ofDim[Double](size.reduce(_ * _)))

    def binary(left: ShapedArray, right: ShapedArray, result: ShapedArray, op: BinaryOp) {
        val resultRank = result.size.size
        val leftSize = pad(left.size, resultRank)
        val rightSize = pad(right.size, resultRank)
        val resultSize = result.size.toArray

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

    def unary(original: ShapedArray, result: ShapedArray, op: UnaryOp) {
        val originalArray = original.array
        val resultArray = result.array
        var i = originalArray.size - 1
        op match {
            case IdentityOp =>
                while(i >= 0) {
                    resultArray(i) = 1.0
                    i -= 1
                }                
        }
    }

    def reduce(original: ShapedArray, result: ShapedArray, op: BinaryOp) {
        val resultArray = result.array
        val resultSize = pad(result.size, original.size.size)
        val originalArray = original.array
        val originalSize = original.size.toArray
        var i = originalArray.size - 1
        op match {
            case AddOp =>
                while(i >= 0) {
                    val resultIndex = computeIndex(i, originalSize, resultSize)
                    resultArray(resultIndex) += originalArray(i)
                    i -= 1
                }
            case MultiplyOp => ???
        }
    }

    def newAxis(original: ShapedArray): ShapedArray = ShapedArray(1 :: original.size, original.array)
    def dropAxis(original: ShapedArray): ShapedArray = ShapedArray(original.size.tail, original.array)

    private def pad(size: List[Int], rank: Int): Array[Int] = 
        (size ++ List.fill(rank - size.size)(1)).toArray
}
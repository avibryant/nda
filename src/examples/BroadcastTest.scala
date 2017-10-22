package nda

object BroadcastTest {
    def test[Foo,Bar] = {
        val scalar = One()
        val vector = N[Foo]
        val vector2 = N[Bar]

        val z1: One = Shape.broadcast(scalar, scalar)
        val z2: N[Foo] = Shape.broadcast(vector, vector)
        val z3: N[Foo] = Shape.broadcast(scalar, vector)
        val z4: N[Foo] = Shape.broadcast(vector, scalar)

        //fails to compile
        //val z5 = Shape.broadcast(vector, vector2)

        val matrix1: One By N[Foo] = scalar by vector
        val matrix2: N[Bar] By N[Foo] = vector2 by vector
        val matrix3: N[Foo] By N[Foo] = vector by vector

        val z6: One By N[Foo] = Shape.broadcast(matrix1, matrix1)
        val z7: N[Bar] By N[Foo] = Shape.broadcast(matrix2, matrix2)
        val z8: N[Bar] By N[Foo] = Shape.broadcast(matrix1, matrix2)

        //fails to compile
        //val z9 = Shape.broadcast(matrix2, matrix3)

        val z10: N[Foo] By N[Foo] = Shape.broadcast(vector, matrix1)
        val z11: N[Bar] By N[Foo] = Shape.broadcast(vector2, matrix2)
        val z12: N[Foo] By N[Foo] = Shape.broadcast(vector, matrix3)
        val z13: N[Bar] By N[Foo] = Shape.broadcast(vector2, matrix1)

        //fails to compile
        //val z13b = Shape.broadcast(vector, matrix2)

        val z14: One By N[Foo] = Shape.broadcast(scalar, matrix1)
        val z15: N[Bar] By N[Foo] = Shape.broadcast(scalar, matrix2)
        val z16: N[Foo] By N[Foo] = Shape.broadcast(scalar, matrix3)
        val z17: One By N[Foo] = Shape.broadcast(matrix1, scalar)
        val z18: N[Bar] By N[Foo] = Shape.broadcast(matrix2, scalar)
        val z19: N[Foo] By N[Foo] = Shape.broadcast(matrix3, scalar)
    }
}

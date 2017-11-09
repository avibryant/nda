package nda
import scala.collection.mutable.HashMap

class Gradient[X <: Shape] {
    var parts = List.empty[PartialGradient[X]]

    def register(part: PartialGradient[X]) {
        parts = part :: parts
    }

    def toNDA: NDA[X] = parts.map(_.toNDA).reduce(_ + _)
}

object Gradient {
    def derive[X <: Shape](root: NDA[X], loss: NDA[One]): NDA[X] = {
        val gradients = HashMap.empty[NDA[_], Gradient[_]]
        def gradient[X<:Shape](nda: NDA[X]): Gradient[X] = {
            gradients.getOrElseUpdate(nda, new Gradient[X]).asInstanceOf[Gradient[X]]
        }

        gradient(loss).register(PGOne)

        def register[X<:Shape](nda: NDA[X]) {
            nda match {
                case Variable(name) => ()
                case Constant(value) => ()
                case b @ Binary(left, right, op) =>
                    gradient(left).register(PGLeftBinary(b, gradient(b)))
                    gradient(right).register(PGRightBinary(b, gradient(b)))
                    register(left)
                    register(right)

                case u @ Unary(original, op) =>
                    gradient(original).register(PGUnary(u, gradient(u)))
                    register(original)

                case r @ Reduce(original, op) =>
                    gradient(original).register(PGReduce(r, gradient(r)))
                    register(original)

                case n @ NewAxis(original) =>
                    gradient(original).register(PGNewAxis(n, gradient(n)))
                    register(original)
 
                case d @ DropAxis(original) =>
                    gradient(original).register(PGDropAxis(d, gradient(d)))
                    register(original)
            }
        }

        register(loss)
        gradient(root).toNDA
    }
}
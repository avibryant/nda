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
        
        var visited = Set[NDA[_]]()
        def visit[X<:Shape](nda: NDA[X]) {
            if(!visited.contains(nda)) {
                visited += nda
                nda match {
                    case v: Variable[_] => ()
                    case Constant(value) => ()
                    case b @ Binary(left, right, op, bcast) =>
                        gradient(left).register(PGBinary(b, gradient(b)))
                        gradient(right).register(PGBinary(Binary(right, left, op, bcast.swap), gradient(b)))
                        visit(left)
                        visit(right)

                    case u @ Unary(original, op) =>
                        gradient(original).register(PGUnary(u, gradient(u)))
                        visit(original)

                    case r @ Reduce(original, op, red) =>
                        gradient(original).register(PGReduce(r, gradient(r)))
                        visit(original)

                    case n @ NewAxis(original) =>
                        gradient(original).register(PGNewAxis(n, gradient(n)))
                        visit(original)
    
                    case d @ DropAxis(original) =>
                        gradient(original).register(PGDropAxis(d, gradient(d)))
                        visit(original)
                }
            }
        }

        visit(loss)
        gradient(root).toNDA
    }
}
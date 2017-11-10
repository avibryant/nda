package nda

import scala.collection.mutable.HashMap

case class Session[T](evaluator: Evaluator[T], variables: Map[String,T], cache: HashMap[NDA[_],T]) {
    def feed[X<:Shape](variable: Variable[X], array: Array[Double])(implicit shape: X): Session[T] =
        Session(
            evaluator,
            variables + (variable.name -> evaluator.in(shape.toList, array)), 
            HashMap[NDA[_],T]())
    
    def run(nda: NDA[_]): Array[Double] =
        evaluator.out(forwards(nda))

    def update[X <: Shape](from: Variable[X], to: NDA[X])(implicit shape: X): Session[T] =
        feed(from, run(to))

    private def forwards[_](nda: NDA[_]): T =
        cache.get(nda) match {
            case Some(t) => t
            case None => nda match {
                case Variable(name) => variables(name)
                case Constant(value) => evaluator.in(List(1), Array(value))
                case binary @ Binary(left, right, op) =>
                    val a1 = forwards(left)
                    updateCache(right, nda){a2 => 
                        val result = evaluator.alloc(binary.b.combine(evaluator.size(a1), evaluator.size(a2)))
                        evaluator.binary(a1, a2, result, op)
                        result
                    }

                case Unary(original, op) =>
                    updateCache(original, nda){a => 
                        val result = evaluator.alloc(evaluator.size(a))
                        evaluator.unary(a, result, op)
                        result
                    }

                case r @ Reduce(original, op) =>
                    updateCache(original, nda){a => 
                        val result = evaluator.alloc(r.b.left(evaluator.size(a)))
                        evaluator.reduce(a, result, op)
                        result
                    }

                case NewAxis(original) =>
                    updateCache(original, nda){a => evaluator.newAxis(a)}
 
                case DropAxis(original) =>
                    updateCache(original, nda){a => evaluator.dropAxis(a)}
            }
        }

    private def updateCache(from: NDA[_], to: NDA[_])(fn: T => T): T = {
        val a1 = forwards(from)
        val a2 = fn(a1)
        cache.update(to, a2)
        a2
    }
}

object Session {
    def apply[T](evaluator: Evaluator[T]): Session[T] = Session(evaluator, Map.empty,  HashMap[NDA[_],T]())
}
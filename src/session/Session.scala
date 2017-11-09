package nda

import scala.collection.mutable.HashMap

case class Session[T](evaluator: Evaluator[T], variables: Map[String,ShapedArray[T]], cache: HashMap[NDA[_],ShapedArray[T]]) {
    def feed[X<:Shape](variable: Variable[X], array: Array[T])(implicit size: Size[X]): Session[T] =
        Session(
            evaluator,
            variables + (variable.name -> ShapedArray(size.toList, array)), 
            HashMap[NDA[_],ShapedArray[T]]())
    
    def run(nda: NDA[_]): Array[T] =
        forwards(nda).array

    private def forwards[_](nda: NDA[_]): ShapedArray[T] =
        cache.get(nda) match {
            case Some(t) => t
            case None => nda match {
                case Variable(name) => variables(name)
                case Constant(value) => evaluator.constant(value)
                case Binary(left, right, op) =>
                    val a1 = forwards(left)
                    updateCache(right, nda){a2 => evaluator.binary(a1, a2, op)}

                case Unary(original, op) =>
                    updateCache(original, nda){a => evaluator.unary(a, op)}

                case r @ Reduce(original, op) =>
                    updateCache(original, nda){a => evaluator.reduce(a, op, r.b.left(a.size))}

                case NewAxis(original) =>
                    updateCache(original, nda){a => a.newAxis}
 
                case DropAxis(original) =>
                    updateCache(original, nda){a => a.dropAxis}

            }
        }

    private def updateCache(from: NDA[_], to: NDA[_])(fn: ShapedArray[T] => ShapedArray[T]): ShapedArray[T] = {
        val a1 = forwards(from)
        val a2 = fn(a1)
        cache.update(to, a2)
        a2
    }
}

object Session {
    def apply[T](evaluator: Evaluator[T]): Session[T] = Session(evaluator, Map.empty,  HashMap[NDA[_],ShapedArray[T]]())
}
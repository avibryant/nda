package nda
import scala.collection.mutable.HashMap
/*
case class FullGradient[X](var partialGradients: List[PartialGradient[X,_,_]]) {
    def register(partialGradient: PartialGradient[X,_,_]) {
        partialGradients = partialGradient :: partialGradients
    }

    def toNDA: NDA[X] = partialGradients.map{_.toNDA}.reduce{_ + _}
}

case class PartialGradient[X <: Shape, Z <: Shape, C <: NDA[Z]](child: C, gradient: FullGradient[Z])(
    fn: (C, NDA[Z]) => NDA[X]
) {
    def toNDA: NDA[X] = fn(child, gradient.toNDA)
}

class Gradient(root: NDA[_]) {
    val gradients = HashMap.empty[NDA[_], FullGradient[_]]

    register(root)
    gradients.update(root, Constant(1.0))

    private def addChildToParents(child: NDA[_]) {
        parents(child).foreach{parent =>
            val old = children.getOrElse(parent, Nil)
            children.update(parent, child :: Nil)
            addChildToParents(parent)
        }
    }

    private def parents(child: NDA[_]): List[NDA[_]] {
        child match {
            case Variable(_) => Nil
            case Constant() => Nil
            case Unary(orig, _) => List(orig)
            case Reduce(orig, _) => List(orig)
            case ReduceAll(orig, _) => List(orig)
            case NewAxis(orig, _) => List(orig)
            case Binary(left, right, _) => List(left, right)
        }
    }

    def gradient[X<:Shape](nda: NDA[X]): Option[NDA[X]] = {
        registry.get(nda).map{t => t.asInstanceOf[NDA[X]]}
    }

    def register[X<:Shape](nda: NDA[X], gradient: NDA[X]): Unit = {
        val newGradient =
            get(nda)
                .map{oldGradient => oldGradient + gradient}
                .getOrElse(gradient)

        registry.update(nda, newGradient)
        registerParents(nda, newGradient)
    }

    def registerParents[X<:Shape](nda: NDA[X], gradient: NDA[X]): Unit = {
        nda match {
            
        }`
    }   
}

object Gradient {
    def derive[X<:Shape](from: NDA[X], to: NDA[One]): NDA[X] = {
        val gradient = new Gradient(to)
        gradient.register(to, Constant(1.0))
        gradient.get(from).getOrElse(error("target does not depend on source"))
    }
}
*/
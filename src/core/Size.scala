package nda

case class Size[A <: Shape](toList: List[Int])

trait Cardinality {
    def n: Int
}

object Size {
    def n[A](n: Int): Size[N[A]] = Size[N[A]](List(n))

    implicit def cardinality[A <: Cardinality](implicit card: A): Size[N[A]] = n[A](card.n)
    
    implicit def one: Size[One] = Size[One](List(1))
    
    implicit def by[D <: Dimension,X <: Shape](implicit outer: Size[D], inner: Size[X]): Size[By[D,X]] = 
        Size(outer.toList ++ inner.toList)
}
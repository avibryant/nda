package nda

case class Size[A <: Shape](toList: List[Int])

object Size {
    implicit def dimension[A <: Dimension](implicit dim: A): Size[A] = Size[A](List(dim.n))
        
    implicit def by[D <: Dimension,X <: Shape](implicit outer: Size[D], inner: Size[X]): Size[By[D,X]] = 
        Size(outer.toList ++ inner.toList)
}
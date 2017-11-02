package nda

object Gradient {
    def derive[X<:Shape](from: NDA[X], to: NDA[One]): NDA[X] = from
}
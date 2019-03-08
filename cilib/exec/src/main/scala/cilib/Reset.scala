package cilib
package exec

trait Reset[A] {
  def reset(a: A): A
}

object Reset {
  implicit def positionReset[A]: Reset[Position[A]] =
    new Reset[Position[A]] {
      def reset(a: Position[A]) =
        a.toPoint
    }

  implicit def entityReset[S, A]: Reset[Entity[S, A]] =
    new Reset[Entity[S, A]] {
      def reset(a: Entity[S, A]) =
        cilib.Lenses._position.modify((p: Position[A]) => p.toPoint)(a)
    }
}

package pfennig

import simulacrum._
import scala.language.{implicitConversions,higherKinds}

@typeclass trait Sampleable[F[_]] {
  @op("sample") def sample[A](distribution: F[A])(randomness: Randomness): A
}

package pfennig
package free

import cats.Comonad
import cats.free.Free
import cats.arrow.NaturalTransformation
import cats.std.function._
import scala.language.higherKinds

object distribution {
  sealed trait Generator[A] {
    def sample(implicit randomness: Randomness): A
  }
  final case class Primitive[F[_],A](distribution: F[A], sampleable: Sampleable[F]) extends Generator[A] {
    def sample(implicit randomness: Randomness): A =
      sampleable.sample(distribution)(randomness)
  }

  def distributionCompiler(implicit randomness: Randomness): NaturalTransformation[Generator, Function0] =
  new NaturalTransformation[Generator, Function0] {
    def apply[A](fa: Generator[A]): () => A =
      () => fa.sample(randomness)
  }

  type Distribution[A] = Free[Generator,A]

  implicit class DistributionOps[A](distribution: Distribution[A]) {
    def prepare(implicit randomness: Randomness): () => A =
      distribution.foldMap(distributionCompiler(randomness))

    def sample: A =
      sample(scala.util.Random)

    def sample(implicit randomness: Randomness): A =
      distribution.prepare.apply()
  }
}

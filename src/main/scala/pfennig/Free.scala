package pfennig
package free

import cats.Comonad
import cats.free.Free
import cats.arrow.NaturalTransformation
import cats.std.function._
import scala.language.higherKinds

object distribution {
  // We define this little algebraic data type as we want to hide the type F in
  // Primitive from external users (i.e. we want it to be an existential) as the
  // only operation we care about (sampling) doesn't need this type to be known,
  // and having it known makes life more difficult for us.
  //
  // We can't do this directly by declaring an existential (or at least I don't
  // know how to do it) so we have this arrangement to allow us to use
  // `Generator` externally that hides the type `F`.
  sealed trait Generator[A] {
    def sample(implicit randomness: Randomness): A
  }
  final case class Primitive[F[_],A](distribution: F[A], sampleable: Sampleable[F]) extends Generator[A] {
    def sample(implicit randomness: Randomness): A =
      sampleable.sample(distribution)(randomness)
  }

  /** Compile a Distribution to a function that, when applied, produces a sample. */
  def compile(implicit randomness: Randomness): NaturalTransformation[Generator, Function0] =
  new NaturalTransformation[Generator, Function0] {
    def apply[A](fa: Generator[A]): () => A =
      () => fa.sample(randomness)
  }

  type Distribution[A] = Free[Generator,A]

  implicit class DistributionOps[A](distribution: Distribution[A]) {
    def prepare(implicit randomness: Randomness): () => A =
      distribution.foldMap(compile(randomness))

    def sample: A =
      sample(scala.util.Random)

    def sample(implicit randomness: Randomness): A =
      distribution.prepare.apply()
  }
}

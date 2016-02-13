package pfennig
package free

import cats.Comonad
import cats.free.Free

object Distribution {
  type Distribution[A] = Free[Random,A]

  implicit class DistributionOps[A](distribution: Distribution[A]) {
    def sample: A =
      sample(scala.util.Random)

    def sample(implicit randomness: Randomness): A =
      distribution.run
  }
}

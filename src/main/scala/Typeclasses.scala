trait Semigroup[A]:
  def combine(l: A, r: A): A
  extension (l: A) def <+>(r: A): A = combine(l, r)

object Semigroup:
  given Semigroup[Unit] = (_, _) => ()
  given Semigroup[Int]  = _ + _



trait Show[A]:
  def shown(a: A): String
  extension (a: A) def show: String = shown(a)

object Show:
  given Show[String] = identity(_)
  given Show[Unit]   = _.toString

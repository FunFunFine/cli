import zio.*
import zio.Console.*
import zio.stream.*
import java.nio.file.*
import zio.stream.ZPipeline.WithOut

object DayOne:
  def run = TaskOne.answer.flatMap(answer => print(answer))

object TaskOne:
  val measurements: ZStream[Any, Throwable, Int] =
    ZStream
      .fromFile(Paths.get("measurements.csv"))
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .map(_.toInt)

  def howManyAreLarger(measurements: List[Int]): Int =
    measurements
      .sliding(2)
      .collect { case a :: b :: Nil =>
        (a, b)

      }
      .count((a, b) => b > a)

  val answer =
    measurements.runCollect.map(chunk => howManyAreLarger(chunk.toList))

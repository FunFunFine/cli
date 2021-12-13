import zio.*
import zio.Console.*
import zio.stream.*
import java.nio.file.*

/** Writes to repository directory */
def writeOutput(filename: String)(data: String): ZIO[Any, Throwable, Unit] = 
    def fileSink(path: Path) =
        ZSink
            .fromFile(path)
            .contramapChunks[String](_.flatMap(_.getBytes))

    ZStream(data).run(fileSink(Paths.get("code.txt"))).ignore

/** Reads from repository directory */
def readInput(filename: String): ZIO[Any, Throwable, List[String]] =
  ZStream
    .fromFile(Paths.get(filename))
    .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
    .runCollect
    .map(_.toList)
     

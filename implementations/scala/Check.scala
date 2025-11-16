import java.nio.file.Files

object Check {
  def main(args: Array[String]): Unit = {
    val entries = Option(Files.list(Cid.CidsDir)).getOrElse {
      System.err.println(s"Unable to list cids directory: ${Cid.CidsDir}")
      System.exit(1)
      return
    }

    val sorted = try entries.toArray.map(_.asInstanceOf[java.nio.file.Path]).sorted
    finally entries.close()

    var mismatches = 0
    var count = 0

    sorted.foreach { path =>
      if (!Files.isDirectory(path)) {
        count += 1
        Cid.cidFor(path) match {
          case Left(error) =>
            println(error)
            mismatches += 1
          case Right(expected) =>
            val name = path.getFileName.toString
            if (expected != name) {
              println(s"$name should be $expected")
              mismatches += 1
            }
        }
      }
    }

    if (mismatches > 0) {
      println(s"Found $mismatches mismatched CID file(s).")
      System.exit(1)
    } else {
      println(s"All $count CID files match their contents.")
    }
  }
}

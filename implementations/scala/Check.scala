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

    var mismatches = scala.collection.mutable.ListBuffer[(String, String)]()
    var downloadFailures = scala.collection.mutable.ListBuffer[(String, String)]()
    var count = 0
    val baseUrl = "https://256t.org"

    sorted.foreach { path =>
      if (!Files.isDirectory(path)) {
        count += 1
        val cid = path.getFileName.toString
        val localContent = Files.readAllBytes(path)
        
        // Check local CID file
        Cid.cidFor(path) match {
          case Left(error) =>
            mismatches += ((cid, error))
          case Right(expected) =>
            if (expected != cid) {
              mismatches += ((cid, expected))
            }
        }
        
        // Check downloaded content
        Cid.downloadCid(baseUrl, cid) match {
          case Left(error) =>
            downloadFailures += ((cid, error))
          case Right(result) =>
            if (!result.isValid) {
              downloadFailures += ((cid, result.computed))
            } else if (!java.util.Arrays.equals(result.content, localContent)) {
              downloadFailures += ((cid, "content mismatch with local file"))
            }
        }
      }
    }

    var hasErrors = false

    if (mismatches.nonEmpty) {
      println("Found CID mismatches:")
      mismatches.foreach { case (cid, expected) =>
        println(s"- $cid should be $expected")
      }
      hasErrors = true
    }

    if (downloadFailures.nonEmpty) {
      System.err.println("Found download validation failures:")
      downloadFailures.foreach { case (cid, error) =>
        System.err.println(s"- $cid: $error")
      }
      hasErrors = true
    }

    if (hasErrors) {
      System.exit(1)
    } else {
      println(s"All $count CID files match their contents.")
      println(s"All $count downloaded CIDs are valid.")
    }
  }
}

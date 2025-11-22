import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.Base64

object Cid {
  val BaseDir: Path = Paths.get("").toAbsolutePath.normalize()
  val ExamplesDir: Path = BaseDir.resolve("examples")
  val CidsDir: Path = BaseDir.resolve("cids")

  private val encoder: Base64.Encoder = Base64.getUrlEncoder.withoutPadding()

  private def toBase64Url(data: Array[Byte]): String = encoder.encodeToString(data)

  private def encodeLength(length: Int): String = {
    val buffer = ByteBuffer.allocate(java.lang.Long.BYTES)
    buffer.putLong(length.toLong)
    val bytes = buffer.array()
    toBase64Url(java.util.Arrays.copyOfRange(bytes, 2, bytes.length))
  }

  private def hashSha512(content: Array[Byte]): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-512")
    digest.digest(content)
  }

  def compute(content: Array[Byte]): String = {
    val prefix = encodeLength(content.length)
    val suffix =
      if (content.length <= 64) toBase64Url(content)
      else toBase64Url(hashSha512(content))

    prefix + suffix
  }

  def cidFor(path: Path): Either[String, String] = {
    try {
      val content = Files.readAllBytes(path)
      Right(compute(content))
    } catch {
      case ex: Exception => Left(s"Failed to read ${path.getFileName}: ${ex.getMessage}")
    }
  }

  case class DownloadResult(content: Array[Byte], computed: String, isValid: Boolean)

  def downloadCid(baseUrl: String, cid: String): Either[String, DownloadResult] = {
    try {
      val url = new java.net.URL(s"${baseUrl.stripSuffix("/")}/$cid")
      val conn = url.openConnection().asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setConnectTimeout(10000)
      conn.setReadTimeout(10000)
      
      val responseCode = conn.getResponseCode
      if (responseCode != 200) {
        return Left(s"HTTP $responseCode: ${conn.getResponseMessage}")
      }
      
      val content = conn.getInputStream.readAllBytes()
      val computed = compute(content)
      val isValid = computed == cid
      
      Right(DownloadResult(content, computed, isValid))
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }
  }
}

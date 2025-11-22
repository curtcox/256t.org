import java.net.HttpURLConnection
import java.net.URL
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.Base64
import kotlin.system.exitProcess

private val baseDir: Path = Paths.get("").toAbsolutePath().normalize()
private val cidsDir: Path = baseDir.resolve("cids")

private fun toBase64Url(data: ByteArray): String =
    Base64.getUrlEncoder().withoutPadding().encodeToString(data)

private fun encodeLength(length: Long): String {
    val buffer = ByteArray(6)
    for (i in 0 until 6) {
        buffer[5 - i] = ((length shr (8 * i)) and 0xFF).toByte()
    }
    return toBase64Url(buffer)
}

private fun computeCid(content: ByteArray): String {
    val prefix = encodeLength(content.size.toLong())
    val suffix = if (content.size <= 64) {
        toBase64Url(content)
    } else {
        val digest = MessageDigest.getInstance("SHA-512").digest(content)
        toBase64Url(digest)
    }
    return prefix + suffix
}

private data class DownloadResult(
    val content: ByteArray,
    val computed: String,
    val isValid: Boolean
)

private fun downloadCid(baseUrl: String, cid: String): DownloadResult {
    val url = URL("${baseUrl.trimEnd('/')}/$cid")
    val connection = url.openConnection() as HttpURLConnection
    connection.requestMethod = "GET"
    connection.connectTimeout = 10000
    connection.readTimeout = 10000
    
    val responseCode = connection.responseCode
    if (responseCode != 200) {
        throw Exception("HTTP $responseCode: ${connection.responseMessage}")
    }
    
    val content = connection.inputStream.readBytes()
    val computed = computeCid(content)
    val isValid = computed == cid
    
    return DownloadResult(content, computed, isValid)
}

fun main() {
    val mismatches = mutableListOf<Pair<String, String>>()
    val downloadFailures = mutableListOf<Pair<String, String>>()
    var count = 0
    val baseUrl = "https://256t.org"
    
    Files.list(cidsDir).sorted().forEach { path ->
        if (Files.isDirectory(path)) return@forEach
        count += 1
        val cid = path.fileName.toString()
        val localContent = Files.readAllBytes(path)
        val expected = computeCid(localContent)
        
        // Check local CID file
        if (cid != expected) {
            mismatches.add(cid to expected)
        }
        
        // Check downloaded content
        try {
            val result = downloadCid(baseUrl, cid)
            if (!result.isValid) {
                downloadFailures.add(cid to result.computed)
            } else if (!result.content.contentEquals(localContent)) {
                downloadFailures.add(cid to "content mismatch with local file")
            }
        } catch (e: Exception) {
            downloadFailures.add(cid to (e.message ?: "unknown error"))
        }
    }

    var hasErrors = false

    if (mismatches.isNotEmpty()) {
        println("Found CID mismatches:")
        mismatches.forEach { (cid, expected) ->
            println("- $cid should be $expected")
        }
        hasErrors = true
    }

    if (downloadFailures.isNotEmpty()) {
        System.err.println("Found download validation failures:")
        downloadFailures.forEach { (cid, error) ->
            System.err.println("- $cid: $error")
        }
        hasErrors = true
    }

    if (hasErrors) {
        exitProcess(1)
    }

    println("All $count CID files match their contents.")
    println("All $count downloaded CIDs are valid.")
}
